#!/usr/bin/python
# WANT_JSON

from ansible.module_utils.basic import AnsibleModule

"""
sacctmgr commands:

             Format= Cluster ClusterNodes Start End State Reason  User  Event  CPUCount Duration
list events        - Clusters= Nodes= Start= End= States= Reason= User= Event= MaxCPUs= MinCPUs=
                     All_Clusters All_Time
             Format= Cluster  Name TRES Start End ID
list reservation   - Cluster= Name= Start= End= ID= Nodes=
                                                                        WithAssoc
             Format= Time        Action  Actor Where Info Clusster  ID  Account  User
list transactions  - Start= End= Action= Actor=           Clusters= ID= Account= User=
             Format= Name  Type  ID
list tres          - Name= Type= ID=
                     WithDeleted
             Format= Name  Cluster  User  ID
list wckey         - Name= Cluster= User= ID= End= Start=
                     WithDeleted

             Format= Name  Classification  DefaultQOS  Flags  RPC                    QosLevel  Fairshare                                              GrpTRES  GrpJobs  GrpMemory  GrpNodes  GrpSubmitJob           MaxTRESMins            MaxJobs  MaxNodes  MaxSubmitJobs  MaxWall
add cluster        - Name=                 DefaultQOS=                               QosLevel= Fairshare=                                             GrpTRES= GrpJobs= GrpMemory= GrpNodes= GrpSubmitJob=          MaxTRESMins=           MaxJobs= MaxNodes= MaxSubmitJobs= MaxWall=
modify cluster     -                       DefaultQOS=                               QosLevel= Fairshare=                                             GrpTRES= GrpJobs= GrpMemory= GrpNodes= GrpSubmitJob=          MaxTRESMins=           MaxJobs= MaxNodes= MaxSubmitJobs= MaxWall=
     (where options) Name= Classification=             Flags= 
delete cluster     - Name= Classification= DefaultQOS= Flags= 
list cluster       - Name= Classification= DefaultQOS= Flags= RPC=
                     WOLimits

             Format= Name  Description   Id  PreemptMode  Flags  GraceTime  GrpJobs  GrpSubmitJob          GrpTRESMins                                GrpTRES  GrpWall  MaxTRESMins  MaxTRESPerJob  MaxTRESPerNode  MaxTRESPerUser  MaxJobs  MaxSubmitJobsPerUser  MaxWall  Preempt  Priority  UsageFactor  UsageThreshold
add qos            - Name= Description=      PreemptMode= Flags= GraceTime= GrpJobs= GrpSubmitJob=         GrpTRESMins=                               GrpTRES= GrpWall= MaxTRESMins= MaxTRESPerJob= MaxTRESPerNode= MaxTRESPerUser= MaxJobs= MaxSubmitJobsPerUser= MaxWall= Preempt= Priority= UsageFactor= UsageThreshold=
modify qos         - Name= Description=      PreemptMode= Flags= GraceTime= GrpJobs= GrpSubmitJob=         GrpTRESMins=                               GrpTRES= GrpWall= MaxTRESMins= MaxTRESPerJob= MaxTRESPerNode= MaxTRESPerUser= MaxJobs= MaxSubmitJobsPerUser= MaxWall= Preempt= Priority= UsageFactor= UsageThreshold= RawUsage=
     (where options) Name= Descriptions= ID= PreemptMode=
delete qos         - Name= Descriptions= ID= PreemptMode=
list qos           - Name= Descriptions= Id= PreemptMode=
                     WithDeleted

                                                                                               WithClusters
             Format= Name  Description  Count  Flags  Id  ServerType  Server  Type             Clusters  Allocated       
add resource       - Name= Description= Count= Flags=     ServerType= Server= Type=            Clusters= PercentAllowed= 
modify resource    -                    Count= Flags=                                 Manager=           PercentAllowed= 
     (where options) Name=                                            Server=                  Clusters=                 
delete resource    - Name=                                                                     Clusters= 
list resource      - Name= Description=        Flags= Id= ServerType= Server=                  Clusters= PercentAllowed= 

                                                      WithAssoc
             Format= Name  Description  Organization  ParentName User  Clusters  DefaultQOS         QOSLevel  Fairshare  GrpTRESMins  GrpTRESRunMins  GrpTRES  GrpJobs  GrpMemory  GrpNodes  GrpSubmitJob  GrpWall  MaxTRESMins  MaxTRES  MaxJobs  MaxNodes  MaxSubmitJobs  MaxWall
add account        - Name= Description= Organization= Parent=          Clusters= DefaultQOS=        QosLevel= Fairshare= GrpTRESMins=                 GrpTRES= GrpJobs= GrpMemory= GrpNodes= GrpSubmitJob= GrpWall= MaxTRESMins= MaxTRES= MaxJobs= MaxNodes= MaxSubmitJobs= MaxWall=
modify account     - Name= Description= Organization= Parent=                    DefaultQOS=        QosLevel= Fairshare= GrpTRESMins= GrpTRESRunMins= GrpTRES= GrpJobs= GrpMemory= GrpNodes= GrpSubmitJob= GrpWall= MaxTRESMins= MaxTRES= MaxJobs= MaxNodes= MaxSubmitJobs= MaxWall= RawUsage=
     (where options) Name= Description= Organization= Parent=    User= Clusters= DefaultQOS=        QosLevel=
delete account     - Name= Description= Organization= Parent=          Clusters= DefaultQOS= 
list account       - Name= Description= Organization= Parent=                                
                     WithDeleted WithCoordinators WithRawQOS WOPLimits

                                                       WithAssoc
             Format= Name  DefaultAccount  AdminLevel  Account  Clusters  Partition  DefaultQOS  DefaultWCKey  QosLevel  Fairshare                                                                                             MaxTRESMins  MaxTRES  MaxJobs  MaxNodes  MaxSubmitJobs  MaxWall 
add user           - Name= DefaultAccount= AdminLevel= Account= Clusters= Partition= DefaultQOS= DefaultWCKey= QosLevel= Fairshare=                                                                                            MaxTRESMins= MaxTRES= MaxJobs= MaxNodes= MaxSubmitJobs= MaxWall=
modify user        -       DefaultAccount= AdminLevel=                               DefaultQOS= DefaultWCKey= QosLevel= Fairshare=                                                                                            MaxTRESMins= MaxTRES= MaxJobs= MaxNodes= MaxSubmitJobs= MaxWall= RawUsage= 
     (where options) Name= DefaultAccount= AdminLevel= Account= Clusters= Partition=                           QosLevel=                                                                                                                                                                       
delete user        - Name= DefaultAccount= AdminLevel= Account= Clusters=                        DefaultWCKey= 
list user          - Name= DefaultAccount= AdminLevel=                                           DefaultWCKey= QosLevel=
                     WithCoordinators WithDeleted WithRawQOS WOPLimits

not yet implemented:

list runawayjobs   - None, strange header
modify job         - DerivedExitCode= Comment=
     (where options) JobID= Cluster=

add coordinator    - Accounts= Names=
delete coordinator - Accounts= Names=

list associations  - Accounts= Clusters= ID= OnlyDefaults Partitions= Parent= Tree Users=
                     Format= WithSubAccounts WithDeleted WOLimits WOPInfo WOPLimits

archive dump       - Directory= Events Jobs PurgeEventAfter= PurgeJobAfter= PurgeStepAfter= PurgeSuspendAfter= Script= Steps Suspend
archive load       - File= or Insert=
"""

class Args(list):
    """A special list for slurm command line key=value arguments."""
    def add(self, field, value=None):
        self.append(field + '=' + value if value else field)

class Parser(object):
    """Generic argument parser"""
    def editable(self):
        return False

    def format(self, sacctmgr):
        pass

    def parse(self, sacctmgr):
        pass

    def sets(self, sacctmgr):
        pass

class Param(Parser):
    """Single argument parameter"""
    def __init__(self, name):
        self.name = name.lower()

    def parse(self, sacctmgr):
        self.val = sacctmgr.params.pop(self.name, None)

    def set(self, sacctmgr):
        sacctmgr.sets.add(self.name, self.val)

class Fmt(Param):
    """Parameter that can also be read"""
    def __init__(self, name, fmt=None):
        Param.__init__(self, name)
        self.fmt = fmt.lower() if fmt else self.name

    def format(self, sacctmgr):
        sacctmgr.format.append(self.fmt)

    def parse(self, sacctmgr):
        Param.parse(self, sacctmgr)

    def cur(self, sacctmgr):
        return [r[self.name] for r in sacctmgr.cur]

class RO(Fmt):
    """Parameter that can only be read"""
    pass

class Filt(Param):
    """Parameter that can only filter results"""
    def parse(self, sacctmgr):
        Param.parse(self, sacctmgr)
        if self.val:
            sacctmgr.filter.add(self.name, self.val)

class RF(RO, Filt):
    """Parameter that can read and filter results"""
    def parse(self, sacctmgr):
        Filt.parse(self, sacctmgr)

class Key(RF):
    """Required, primary key parameter"""
    def editable(self):
        return True

    def parse(self, sacctmgr):
        RF.parse(self, sacctmgr)
        if not self.val and sacctmgr.state != 'list':
            sacctmgr.fail('missing required argument: %s' % self.name)

class RW(Fmt):
    """Parameter than can be read and modified"""
    def editable(self):
        return True

    def eq(self, val):
        return self.val == val

    def parse(self, sacctmgr):
        if sacctmgr.state != 'present':
            return
        Fmt.parse(self, sacctmgr)

    def sets(self, sacctmgr):
        if self.val is None:
            return
        cur = self.cur(sacctmgr)
        if not cur or not all(map(self.eq, cur)):
            self.set(sacctmgr)

class Act(Param):
    """Parameter than causes an action"""
    def parse(self, sacctmgr):
        if sacctmgr.state != 'present':
            return
        Param.parse(self, sacctmgr)

    def sets(self, sacctmgr):
        if self.val is None:
            return
        if sacctmgr.cur:
            self.set(sacctmgr)

class List(Parser):
    """Set of parameters"""
    def __init__(self, *l):
        self.list = l

    def editable(self):
        return self.list[0].editable()

    def format(self, sacctmgr):
        for p in self.list:
            p.format(sacctmgr)

    def parse(self, sacctmgr):
        for p in self.list:
            p.parse(sacctmgr)

    def sets(self, sacctmgr):
        for p in self.list:
            p.sets(sacctmgr)

class With(Parser):
    """Parameters dependent on a With* argument, only supplied if the given key parameter is"""
    def __init__(self, w, k, *l):
        self.args = w
        self.key = k
        self.sub = List(*l)

    def parse(self, sacctmgr):
        self.key.parse(sacctmgr)
        if self.key.val is not None:
            sacctmgr.args.append(self.args)
            self.sub.format(sacctmgr)
            self.sub.parse(sacctmgr)

    def sets(self, sacctmgr):
        if self.key.val is not None:
            self.sub.sets(sacctmgr)

class Opt(Param):
    """Optional flag controlling list results"""
    def parse(self, sacctmgr):
        if sacctmgr.state != 'list':
            return
        Param.parse(self, sacctmgr)
        if self.val is None:
            return
        try:
            self.val = sacctmgr.module._check_type_bool(self.val)
        except (TypeError, ValueError):
            sacctmgr.fail(msg="argument %s could not be converted to bool" % self.name)
        if self.val:
            sacctmgr.args.append(self.name)

ENTITIES = dict(
    cluster         = List(Key('Name', 'Cluster'), RF('Classification'),
        RW('DefaultQOS'), RO('Flags'), RO('RPC'), RW('QosLevel'), RW('Fairshare'), RW('GrpTRES'), RW('GrpJobs'), RW('GrpMemory'), RW('GrpNodes'), RW('GrpSubmitJob'), RW('MaxTRESMins'), RW('MaxJobs'), RW('MaxNodes'), RW('MaxSubmitJobs'), RW('MaxWall')),
    qos             = List(Key('Name'),
        RW('Description'), RO('Id'), RW('PreemptMode'), RW('Flags'), RW('GraceTime'), RW('GrpJobs'), RW('GrpSubmitJob'), RW('GrpTRESMins'), RW('GrpTRES'), RW('GrpWall'), RW('MaxTRESMins'), RW('MaxTRESPerJob'), RW('MaxTRESPerNode'), RW('MaxTRESPerUser'), RW('MaxJobs'), RW('MaxSubmitJobsPerUser'), RW('MaxWall'), RW('Preempt'), RW('Priority'), RW('UsageFactor'), RW('UsageThreshold'),
        Act('RawUsage'), Opt('WithDeleted')),
    resource        = List(Key('Name'),
        RW('Description'), RW('Count'), RW('Flags'), RO('Id'), RW('ServerType'), RW('Server'), RW('Type'),
        With('WithClusters', RF('Cluster'), RW('PercentAllowed', 'Allocated'))),
    account         = List(Key('Name', 'Account'),
        RW('Description'), RW('Organization'),
        With('WithAssoc', RF('Parent', 'ParentName'), RF('Cluster'), RW('DefaultQOS'), RW('QOSLevel'), RW('Fairshare'), RW('GrpTRESMins'), RW('GrpTRESRunMins'), RW('GrpTRES'), RW('GrpJobs'), RW('GrpMemory'), RW('GrpNodes'), RW('GrpSubmitJob'), RW('GrpWall'), RW('MaxTRESMins'), RW('MaxTRES'), RW('MaxJobs'), RW('MaxNodes'), RW('MaxSubmitJobs'), RW('MaxWall'),
            Act('RawUsage')),
        Opt('WithDeleted')),
    user            = List(Key('Name', 'User'),
        RW('DefaultAccount'), RW('AdminLevel'),
        With('WithAssoc', RF('Account'), RF('Cluster'), RF('Partition'), RW('DefaultQOS'), RW('DefaultWCKey'), RW('QosLevel'), RW('Fairshare'), RW('MaxTRESMins'), RW('MaxTRES'), RW('MaxJobs'), RW('MaxNodes'), RW('MaxSubmitJobs'), RW('MaxWall'),
            Act('RawUsage')),
        Opt('WithDeleted')),
    events          = List(RF('Cluster'), RF('Nodes', 'ClusterNodes'), RF('Start'), RF('End'), RF('State'), RF('Reason'), RF('User'), RF('Event'), RO('CPUCount'), RO('Duration'),
        Filt('Start'), Filt('End'), Filt('MaxCPUs'), Filt('MinCPUs'), Opt('All_Clusters'), Opt('All_Time')),
    reservation     = List(RF('Name'), RF('Cluster'), RO('TRES'), RF('Start'), RF('End'), RF('ID'),
        Filt('Nodes')),
    transactions    = List(RO('Time'), RF('Action'), RF('Actor'), RO('Where'), RO('Info'), RF('Clusster'), RF('ID'),
        With('WithAssoc', RF('Account'), RF('User'))),
    tres            = List(RF('Name'), RF('Type'), RF('ID'),
        Opt('WithDeleted')),
    wckey           = List(RF('Name'), RF('Cluster'), RF('User'), RF('ID'),
        Filt('End'), Filt('Start'), Opt('WithDeleted')),
)

class SAcctMgr(object):
    def __init__(self):
        self.module = AnsibleModule(
            argument_spec = dict(
                state = dict(choices=['present', 'absent', 'list']),
                entity = dict(required=True, choices=ENTITIES.keys()),
            ),
            check_invalid_arguments = False,
            supports_check_mode = True
        )
        self.bin = self.module.get_bin_path('sacctmgr', True)
        self.params = self.module.params
        self.entity = self.params.pop('entity')

        self.result = {}
        self.format = []
        self.filter = Args()
        self.args = Args()
        self.sets = Args()

    def exit(self, **args):
        for (k, v) in args:
            self.result[k] = v
        self.module.exit_json(**self.result)

    def fail(self, msg):
        self.result['msg'] = msg
        self.module.fail_json(**self.result)

    def change(self):
        """Register a change and possibly exit in check mode."""
        self.result['changed'] = True
        if self.module.check_mode:
            self.exit()
        
    def cmd(self, readonly, *args):
        cmd = [self.bin]
        if readonly:
            cmd.append('-r')
        else:
            #self.fail(" ".join(args))
            cmd.append('-i')
            self.change()
        cmd.extend(args)
        (_, o, e) = self.module.run_command(cmd, check_rc=True)
        if e != '':
            self.fail(e)
        return o

    def list(self):
        """Parse the output of a list command."""
        cmd = ['-nP', 'list', self.entity, 'format=' + ','.join(self.format)] + self.args + self.filter
        l = [r.split('|') for r in self.cmd(True, *cmd).splitlines()]
        n = len(self.format)
        if any(len(r) != n for r in l):
            self.fail('unexpected list output for %s' % self.entity)
        return [dict(zip(self.format, r)) for r in l]

    def create(self):
        cmd = ['add', self.entity] + self.filter + self.sets
        self.cmd(False, *cmd)

    def modify(self):
        cmd = ['modify', self.entity] + self.filter + ["set"] + self.sets
        self.cmd(False, *cmd)

    def delete(self):
        cmd = ['delete', self.entity] + self.filter
        self.cmd(False, *cmd)

    def main(self):
        parser = ENTITIES[self.entity]
        editable = parser.editable()
        self.state = self.params.pop('state', None) or ('present' if editable else 'list')
        if not editable and self.state != 'list':
            self.fail('cannot set state=%s for %s' % (self.state, self.entity))
        parser.format(self)
        parser.parse(self)
        if self.params:
            self.fail('unhandled arguments: %s' % ','.join(self.params.iterkeys()))
        self.cur = self.list()
        self.result[self.entity] = self.cur
        if self.state == 'list':
            pass
        elif self.state == 'present':
            parser.sets(self)
            if not self.cur:
                self.create()
            elif self.sets:
                self.modify()
        elif self.state == 'absent':
            if self.cur:
                self.delete()
        self.exit()

if __name__ == '__main__':
    SAcctMgr().main()
