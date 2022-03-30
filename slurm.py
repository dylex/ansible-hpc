#!/usr/bin/python

ANSIBLE_METADATA = {
    'metadata_version': '1.1',
    'status': ['preview'],
    'supported_by': 'community'
}

DOCUMENTATION = """
---
module: slurm
short_description: Manage slurm clusters
author: Dylan Simon (@dylex)
description:
    - Provide an interface to sacctmgr, mostly mirrors the command-line interface
options:
    state:
        description:
            - The action to take, either add/modify, delete, or list.
            - Equivalent to the first argument to sacctmgr.
        choices: ["present", "absent", "list"]
        default: present or list
    entity:
        description:
            - The type of entity to list or modify.
            - Equivalent to the second argument to sacctmgr.
            - To manipulate associations, specify "parent=" with account, "account=" with user, "cluster=" with resource, or "account=" with transactions.
        required: true
        choices: ["cluster", "qos", "resource", "account", "user", "events", "reservation", "transactions", "tres", "wckey"]
    name:
        description:
            - The name of the entity to modify, for cluster, qos, resource, account, user, reservation, tres, or wckey
        required: false
    args:
        type: dict
        description:
            - Other arguments are the same as to sacctmgr, except all are lower-case.
            - Rather than WithClusters or WithAssoc, if you specify "parent=", "account=", or "cluster=" they will be inferred.
            - For some arguments, sacctmgr may report values in a different format than it accepts them.  In this case, you can specify a dict with C(set) as the value to set, and C(test) as the value to compare against.
            - For TRES values, like sacctmgr, you must explicitly set C(res=-1) to clear resource contraints.  These will be ignored when comparing.
"""

EXAMPLES = """
- name: create slurm qos
  hosts: slurm
  slurm:
    entity: qos
    state: present
    name: defq
    args:
      priority: 10
      maxwall: 7-00:00:00
      grptres: node=1000,mem=10000000M
      maxtresperuser: cpu=1000,node=-1
      gracetime:
        set: 60
        test: 00:01:00

- name: list all user associations
  hosts: slurm
  slurm:
    entity: user
    state: list
    args:
      account: ''

- name: create slurm user association
  hosts: slurm
  slurm:
    entity: user
    state: present
    name: {{user}}
    args:
      account: {{slurm_account}}
"""

RETURN = """
entity_type:
    description: the list of matching entities, before any actions are taken
    type: list
    returned: always
"""

from ansible.module_utils.basic import AnsibleModule

class Args(list):
    """A special list for slurm command line key=value arguments."""
    def add(self, field, value=None):
        self.append(field + ('=' + str(value) if value is not None else ''))

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

    def set(self, sacctmgr, val):
        sacctmgr.sets.add(self.name, val)

class Fmt(Param):
    """Parameter that can also be read"""
    def __init__(self, name, fmt=None):
        super(Fmt, self).__init__(name)
        self.fmt = fmt.lower() if fmt else self.name

    def format(self, sacctmgr):
        sacctmgr.format.append(self.fmt)

    def parse(self, sacctmgr):
        super(Fmt, self).parse(sacctmgr)

    def cur(self, sacctmgr):
        return [r[self.name] for r in sacctmgr.cur]

class RO(Fmt):
    """Parameter that can only be read"""
    pass

class Filt(Param):
    """Parameter that can only filter results"""
    def parse(self, sacctmgr):
        super(Filt, self).parse(sacctmgr)
        if self.val:
            sacctmgr.keys.append(self.fmt)
            sacctmgr.filter.add(self.name, self.val)

class RF(RO, Filt):
    """Parameter that can read and filter results"""
    def parse(self, sacctmgr):
        super(RF, self).parse(sacctmgr)

class Key(RF):
    """Required, primary key parameter"""
    def editable(self):
        return True

    def parse(self, sacctmgr):
        super(Key, self).parse(sacctmgr)
        if not self.val and sacctmgr.state != 'list':
            sacctmgr.fail('missing required argument: %s' % self.name)

class RW(Fmt):
    """Parameter that can be read and modified"""
    def editable(self):
        return True

    def eq(self, val):
        return str(self.val) == val

    def parse(self, sacctmgr):
        if sacctmgr.state == 'present':
            super(RW, self).parse(sacctmgr)
            if type(self.val) is dict:
                self.set_val = self.val['set']
                self.val = self.val['test']
            else:
                self.set_val = self.val
        elif sacctmgr.state == 'absent':
            sacctmgr.params.pop(self.name, None)

    def sets(self, sacctmgr):
        if self.set_val is None:
            return
        cur = self.cur(sacctmgr)
        if not cur or not all(map(self.eq, cur)):
            self.set(sacctmgr, self.set_val)

class RWSet(RW):
    def eq(self, val):
        return set(self.val.split(',')) == set(val.split(','))

class TRES(RWSet):
    def parse(self, sacctmgr):
        super(TRES, self).parse(sacctmgr)
        try:
            self.val = ','.join(v for v in self.val.split(',') if not v.endswith('=-1'))
        except AttributeError:
            pass

class Act(Param):
    """Parameter that causes an action"""
    def parse(self, sacctmgr):
        if sacctmgr.state == 'present':
            super(Act, self).parse(sacctmgr)
        elif sacctmgr.state == 'absent':
            sacctmgr.params.pop(self.name, None)

    def sets(self, sacctmgr):
        if self.val is None:
            return
        if sacctmgr.cur:
            self.set(sacctmgr, self.val)

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
        self.key.format(sacctmgr)
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
        super(Opt, self).parse(sacctmgr)
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
        RW('DefaultQOS'), RO('Flags'), RO('RPC'), RW('QosLevel'), RW('Fairshare'), TRES('GrpTRES'), RW('GrpJobs'), RW('GrpMemory'), RW('GrpNodes'), RW('GrpSubmitJob'), RW('MaxTRESMins'), RW('MaxJobs'), RW('MaxNodes'), RW('MaxSubmitJobs'), RW('MaxWall')),
    qos             = List(Key('Name'),
        RW('Description'), RO('Id'), RW('PreemptMode'), RW('Flags'), RW('GraceTime'), RW('GrpJobs'), RW('GrpSubmitJob'), RW('GrpTRESMins'), TRES('GrpTRES'), RW('GrpWall'), RW('MaxTRESMins'), TRES('MaxTRESPerJob'), TRES('MaxTRESPerNode'), TRES('MaxTRESPerUser'), RW('MaxJobs'), RW('MaxSubmitJobsPerUser'), RW('MaxWall'), RW('Preempt'), RW('Priority'), RW('UsageFactor'), RW('UsageThreshold'),
        Act('RawUsage'), Opt('WithDeleted'),
        TRES('MaxTRES'), RW('MaxJobsPerUser'), RW('MaxJobsAccruePerUser'), TRES('MinTRESPerJob')),
    resource        = List(Key('Name'),
        RW('Description'), RW('Count'), RW('Flags'), RO('Id'), RW('ServerType'), RW('Server'), RW('Type'),
        With('WithClusters', RF('Cluster'), RW('PercentAllowed', 'Allocated'))),
    account         = List(Key('Name', 'Account'),
        RW('Description'), RW('Organization'),
        With('WithAssoc', RF('Parent', 'ParentName'), RF('Cluster'), RW('DefaultQOS'), RW('QOSLevel'), RW('Fairshare'), RW('GrpTRESMins'), RW('GrpTRESRunMins'), TRES('GrpTRES'), RW('GrpJobs'), RW('GrpMemory'), RW('GrpNodes'), RW('GrpSubmitJob'), RW('GrpWall'), RW('MaxTRESMins'), TRES('MaxTRES'), RW('MaxJobs'), RW('MaxNodes'), RW('MaxSubmitJobs'), RW('MaxWall'),
            Act('RawUsage')),
        Opt('WithDeleted')),
    user            = List(Key('Name', 'User'),
        RW('DefaultAccount'), RW('AdminLevel'),
        With('WithAssoc', RF('Account'), RF('Cluster'), RF('Partition'), RW('DefaultQOS'), RW('DefaultWCKey'), RWSet('QosLevel'), RW('Fairshare'), RW('MaxTRESMins'), TRES('MaxTRES'), RW('MaxJobs'), RW('MaxNodes'), RW('MaxSubmitJobs'), RW('MaxWall'),
            Act('RawUsage')),
        Opt('WithDeleted')),
    events          = List(RF('Cluster'), RF('Nodes', 'ClusterNodes'), RF('Start'), RF('End'), RF('State'), RF('Reason'), RF('User'), RF('Event'), RO('CPUCount'), RO('Duration'),
        Filt('Start'), Filt('End'), Filt('MaxCPUs'), Filt('MinCPUs'), Opt('All_Clusters'), Opt('All_Time')),
    reservation     = List(RF('Name'), RF('Cluster'), RO('TRES'), RF('Start'), RF('End'), RF('ID'),
        Filt('Nodes')),
    transactions    = List(RO('Time'), RF('Action'), RF('Actor'), RO('Where'), RO('Info'), RF('Cluster'), RF('ID'),
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
                name = dict(type='str'),
                args = dict(type='dict', default={}),
            ),
            supports_check_mode = True
        )
        self.bin = self.module.get_bin_path('sacctmgr', True, ['/opt/slurm/bin', '/cm/shared/apps/slurm/current/bin'])
        self.entity = self.module.params['entity']
        self.params = self.module.params['args']
        try:
            self.params['name'] = self.module.params['name']
        except KeyError:
            pass

        self.result = {}
        self.format = []
        self.keys = []
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
        #return list(filter(lambda d: all(d[k] for k in self.keys), map(lambda r: dict(zip(self.format, r)), l)))
        return [d for d in (dict(zip(self.format, r)) for r in l) if all(d[k] for k in self.keys)]

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
        self.state = self.module.params.get('state') or ('present' if editable else 'list')
        if not editable and self.state != 'list':
            self.fail('cannot set state=%s for %s' % (self.state, self.entity))
        parser.format(self)
        parser.parse(self)
        if self.params:
            self.fail('unhandled arguments: %s' % ','.join(self.params.keys()))
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
