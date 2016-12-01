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

data Node = Node
  { nodeName :: !T.Text
  , nodeId :: Int
  , nodeCluster :: Maybe T.Text
  , nodeDomain :: Maybe T.Text
  , nodeGroups :: [T.Text]
  , nodeEnabled :: Bool
  , nodeNetDevs :: Map T.Text [(T.Text, T.Text)]
  } deriving (Show)

parseNode :: String -> [(T.Text, String)] -> Maybe Node
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

parseNodes :: [String] -> [Node]
parseNodes [] = []
parseNodes (('#':'#':'#':'#':' ':(reverse -> '#':(dropWhile ('#'==) -> ' ':(reverse -> n))))
  : (mapMaybeWhile (splitEq <=< stripPrefix (n++": ") . dropWhile (' ' ==)) ->
    (parseNode n . map (first $ T.dropWhileEnd (' '==) . T.pack) -> Just a, r))) =
  a : parseNodes r
parseNodes (s:_) = error $ "error parsing ww output at line: " ++ s

getNodes :: [String] -> IO [Node]
getNodes args = parseNodes . lines <$> readProcess "wwsh" ("node" : "print" : args) ""

variables :: Node -> JSON.Value
variables Node{..} = JSON.object
  $ maybeList ("warewulf_domain" JSON..=) nodeDomain
  [ "warewulf_id" JSON..= nodeId
  , "warewulf_netdevs" JSON..= Map.map (JSON.object . map (uncurry (JSON..=))) nodeNetDevs
  ]

groups :: Node -> [T.Text]
groups Node{..} =
  (if nodeEnabled then ("enabled" :) else id)
  $ maybeList id nodeCluster $ nodeGroups

run :: Option -> IO JSON.Value
run OptionList = do
  nl <- getNodes []
  return $ JSON.Object
    $ Map.insert "_meta" (JSON.object
      [ "hostvars" JSON..= JSON.object
        (map (\n -> nodeName n JSON..= variables n) nl)
      ])
    $ Map.insert "warewulf" (JSON.toJSON $ map nodeName nl)
    $ Map.map JSON.toJSON $ mapUnion (\n -> Map.fromList $ map ((, [nodeName n])) $ groups n) nl
run (OptionHost h) = do
  nl <- getNodes [h]
  case nl of
    [] -> fail "No matching host"
    [n] -> return $ variables n
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
