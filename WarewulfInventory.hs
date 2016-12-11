{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (first)
import           Control.Monad ((<=<))
import qualified Data.Aeson.Types as JSON
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map
import           Data.List (foldl', stripPrefix)
import qualified Data.Text as T
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (stderr, hPutStrLn)
import           System.Process (readProcess)
import           Text.Read (readMaybe)

data Option
  = OptionList
  | OptionHost String

options :: [Opt.OptDescr Option]
options =
  [ Opt.Option [] ["list"]
      (Opt.NoArg OptionList)
      "List inventory"
  , Opt.Option [] ["host"]
      (Opt.ReqArg OptionHost "HOST")
      "List host variables"
  ]

type Map = Map.HashMap -- Map
type Mappable k = (Hashable k, Eq k) -- Ord k

maybeList :: (a -> b) -> Maybe a -> [b] -> [b]
maybeList = maybe id . (.) (:)

mapMaybeWhile :: (a -> Maybe b) -> [a] -> ([b], [a])
mapMaybeWhile f ((f -> Just a):l) = first (a:) $ mapMaybeWhile f l
mapMaybeWhile _ l = ([], l)

mapUnionWith :: Mappable k => (v -> v -> v) -> (a -> Map k v) -> [a] -> Map k v
mapUnionWith u f = foldl' (\m -> Map.unionWith u m . f) Map.empty

mapUnion :: (Mappable k, Monoid v) => (a -> Map k v) -> [a] -> Map k v
mapUnion = mapUnionWith mappend

splitEq :: String -> Maybe (String, String)
splitEq (' ':'=':' ':r) = return ([], r)
splitEq (c:r) = first (c:) <$> splitEq r
splitEq [] = Nothing

unDef :: String -> Maybe T.Text
unDef "UNDEF" = Nothing
unDef s = Just $ T.pack s

data Object
  = Node
    { objectName :: !T.Text
    , objectId :: !Int
    , nodeCluster :: Maybe T.Text
    , nodeDomain :: Maybe T.Text
    , nodeGroups :: [T.Text]
    , nodeEnabled :: !Bool
    , nodeNetDevs :: Map T.Text [(T.Text, T.Text)]
    }
  | VNFS
    { objectName :: !T.Text
    -- objectId :: !Int -- not included in wwsh vnfs list
    , vnfsSize :: !Float
    , vnfsPath :: !T.Text
    }
  deriving (Show)

parseNode :: String -> [(T.Text, String)] -> Maybe Object
parseNode n d = Node (T.pack n)
  <$> (readMaybe =<< get "ID")
  <*> (unDef <$> get "CLUSTER")
  <*> (unDef <$> get "DOMAIN")
  <*> (maybe [] (T.split (','==)) . unDef <$> get "GROUPS")
  <*> (bool =<< get "ENABLED")
  <*> Just (mapUnion netdev d)
  where
  get k = lookup k d
  bool "TRUE" = Just True
  bool "FALSE" = Just False
  bool _ = Nothing
  netdev (T.split ('.'==) -> [i, k], unDef -> Just v) = Map.singleton i [(T.toLower k, v)]
  netdev _ = Map.empty

parseNodes :: [String] -> [Object]
parseNodes [] = []
parseNodes (('#':'#':'#':'#':' ':(reverse -> '#':(dropWhile ('#'==) -> ' ':(reverse -> n))))
  : (mapMaybeWhile (splitEq <=< stripPrefix (n++": ") . dropWhile (' ' ==)) ->
    (parseNode n . map (first $ T.dropWhileEnd (' '==) . T.pack) -> Just a, r))) =
  a : parseNodes r
parseNodes (s:_) = error $ "error parsing wwsh node output at line: " ++ s

getNodes :: [String] -> IO [Object]
getNodes args = parseNodes . lines <$> readProcess "wwsh" ("node" : "print" : args) ""

splitVNFS :: String -> Maybe (String, (Float, String))
splitVNFS (' ':(reads -> [(z, ' ':(dropWhile (' '==) -> p@('/':_)))])) = return ([], (z, p))
splitVNFS (c:r) = first (consStrip c) <$> splitVNFS r where
  consStrip ' ' [] = []
  consStrip h t = h : t
splitVNFS [] = Nothing

parseVNFS :: String -> Object
parseVNFS (splitVNFS -> Just (unDef -> Just n, (z, unDef -> Just p))) = VNFS n z p
parseVNFS s = error $ "error parsing wwsh vnfs output line: " ++ s

parseVNFSs :: [String] -> [Object]
parseVNFSs ((words -> ["VNFS", "NAME", "SIZE", "(M)", "CHROOT", "LOCATION"]):l) = map parseVNFS l
parseVNFSs (s:_) = error $ "error parsing wwsh vnfs header: " ++ s
parseVNFSs [] = []

variables :: Object -> JSON.Value
variables Node{..} = JSON.object
  $ maybeList ("warewulf_domain" JSON..=) nodeDomain
  [ "warewulf_id" JSON..= objectId
  , "warewulf_netdevs" JSON..= Map.map (JSON.object . map (uncurry (JSON..=))) nodeNetDevs
  ]
variables VNFS{..} = JSON.object
  [ "warewulf_vnfs_size" JSON..= vnfsSize
  , "ansible_host" JSON..= vnfsPath
  , "ansible_connection" JSON..= ("chroot" :: T.Text)
  ]

memberships :: Object -> [T.Text]
memberships Node{..} = "warewulf_node" :
  (if nodeEnabled then ("enabled" :) else id)
  (maybeList id nodeCluster $ nodeGroups)
memberships VNFS{} = ["warewulf_vnfs"]

getVNFS :: [String] -> IO [Object]
getVNFS args = parseVNFSs . lines <$> readProcess "wwsh" ("vnfs" : "list" : args) ""

getObjects :: [String] -> IO [Object]
getObjects args = (++) <$> getNodes args <*> getVNFS args

run :: Option -> IO JSON.Value
run OptionList = do
  l <- getObjects []
  return $ JSON.Object
    $ Map.insert "_meta" (JSON.object
      [ "hostvars" JSON..= JSON.object
        (map (\n -> objectName n JSON..= variables n) l)
      ])
    $ Map.map JSON.toJSON $ mapUnion (\n -> Map.fromList $ map ((, [objectName n])) $ memberships n) l
run (OptionHost h) = do
  l <- getObjects [h]
  case l of
    [] -> fail "No matching host"
    [o] -> return $ variables o
    _ -> fail "Multiple matching hosts"

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case Opt.getOpt Opt.Permute options args of
    ([o], [], []) -> do
      BSLC.putStrLn . encodePretty =<< run o
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " OPTION\n\
        \Ansible dynamic inventory from Warewulf node database\n\
        \https://github.com/dylex/ansible-warewulf-inventory\n")
        options
      exitFailure
