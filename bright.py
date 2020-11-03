#!/cm/local/apps/python3/bin/python3
# -*- coding: utf-8 -*-

from __future__ import absolute_import, division, print_function
__metaclass__ = type

ANSIBLE_METADATA = {
    'metadata_version': '1.1',
    'status': ['preview'],
    'supported_by': 'community'
}

DOCUMENTATION = """
---
module: bright
short_description: Bright cm
description:
  - Manage Bright Cluster Management entities.
notes:
  - This was created mainly to manage Slurm configuration in Bright 9.0, and has been tested mainly for those types of entities, but should theoretically work for almost any Bright settings.  It uses the C(pythoncm) Bright interface, so see the Bright Developer documentation for naming and typing conventions.
  - You will likely have to set C(ansible_python_interpreter=/cm/local/apps/python3/bin/python3) if running this on a bright node.
  - Since bright is picky about types, if you use templates to set values, you may want to set the ansible configuration C(jinja2_native) to preserve types.
author:
  - Dylan Simon (@dylex)
requirements:
  - pythoncm
options:
  name:
    required: true
    description:
      - The name of the entity to be managed.
    type: str
  type:
    required: true
    description:
      - The type (using the pythoncm CamelCase name) of the entity to be managed, e.g., C(PhysicalNode), C(JobQueue), etc.
  state:
    description: Intended state
    choices: [ absent, present ]
    default: present
  clone:
    type: str
    description:
      - When creating an entity C(state=present), clone it from existing entity instead of creating from scratch.
  attrs:
    type: dict
    description:
      - Attributes to set on the entity C(state=present).
      - Referenced entities can be specified by name.  Contained entities can be specified by nested dicts, including C(childType) to create specific types.
      - Lists are replaced entirely.  Lists of contained entities can be selectived updated by dicts keyed on the name of the entity.
    default: {}
"""

EXAMPLES = """
- bright:
    type: SlurmWlmCluster
    name: slurm
    attrs:
      gresTypes: [gpu]
      cgroups:
        constrainCores: true
  vars:
    ansible_python_interpreter: /cm/local/apps/python3/bin/python3

- bright:
    type: SlurmJobQueue
    name: gen
    attrs:
      maxTime: 7-0
      allowAccounts: ALL
      options: [QoS=gen]
- bright:
    type: ConfigurationOverlay
    name: slurm-client-category
    clone: slurm-client
    attrs:
      categories:
        - category1
        - category2
      roles:
        slurmclient:
          childType: SlurmClientRole
          wlmCluster: slurm
          realMemory: 256000
          coresPerSocket: 20
          sockets: 2
          features: [skylake,ib]
          queues: [gen]
          genericResources:
            - alias: gpu0
              name: gpu
              count: '1'
              file: /dev/nvidia0
              type: v100
"""


RETURN = """
name:
  description: resolved name of the entity
  type: str
  returned: when entity exists at any point
type:
  description: specific type of entity
  type: str
  returned: when entity exists at any point
entity:
  description: full entity
  type: dict
  returned: when entity exists at any point
"""

import traceback

from ansible.module_utils.basic import AnsibleModule, missing_required_lib
from ansible.module_utils._text import to_native

CM_IMP_ERR = None
try:
    import pythoncm.cluster

    HAS_CM = True
except ImportError:
    CM_IMP_ERR = traceback.format_exc()
    HAS_CM = False

def getitem(l, i):
    try:
        return l[i]
    except IndexError:
        return None

class Entity(object):
    types = [m for m in dir(pythoncm.entity) if isinstance(getattr(pythoncm.entity, m), type)] if HAS_CM else []

    def __init__(self, module):
        self.module = module
        self.state = module.params['state']
        self.name = module.params['name']
        self.type = module.params['type']
        self.clone = module.params['clone']
        self.attrs = module.params['attrs']

    def absent(self):
        if not self.entity:
            return

        self.result['changed'] = True
        if self.module.check_mode:
            return

        r = self.entity.remove()
        if not r.success:
            self.result['failed'] = True

    def gettype(self, typ):
        return getattr(pythoncm.entity, typ)

    def getentity(self, name, typ):
        e = self.cluster.get_by_name(name, typ)
        if not e:
            raise KeyError("%s:%s"%(typ, name))
        return e

    def makeentity(self, cur, val, field, name=None):
        from pythoncm.entity.meta_data import MetaData
        if val is None:
            return

        elif field.kind == MetaData.RESOLVE:
            if type(val) is not str:
                raise TypeError('Expected %s name, not %r'%(field.instance, val))
            return self.getentity(val, field.instance)

        elif field.kind == MetaData.ENTITY:
            if type(val) is str:
                val = {'name':val}
            if type(val) is not dict:
                raise TypeError('Expected %s attributes, not %r'%(field.instance, val))
            if not cur:
                cur = self.gettype(val.get('childType', val.get('baseType', field.instance)))(cluster = self.cluster)
                self.changed.add(cur)
                if name and hasattr(cur, 'name'):
                    cur.name = name
            self.setentity(cur, val)
            return cur

    def setentity(self, ent, src):
        fields = {f.name: f for f in ent.fields()}
        for k, v in src.items():
            c = getattr(ent, k)
            f = fields[k]
            if f.instance:
                if f.vector:
                    if type(v) is list:
                        v = [self.makeentity(getitem(c, i), x, f) for (i, x) in enumerate(v)]
                    elif type(v) is dict:
                        l = c.copy()
                        for n, e in v.items():
                            try:
                                i = next(i for (i, x) in enumerate(l) if x and x.name == n)
                            except StopIteration:
                                i = len(l)
                                l.append(None)
                            l[i] = self.makeentity(l[i], e, f, n)
                        v = l
                    elif type(v) is str and issubclass(self.gettype(f.instance), pythoncm.entity.Device):
                        from pythoncm.device_selection import DeviceSelection
                        d = DeviceSelection(self.cluster)
                        #d.add_devices_in_text_range(v, True)
                        d.add_devices(pythoncm.namerange.expand.Expand.expand(v), True)
                        v = d.get_sorted_by_name()
                    else:
                        raise TypeError('%s: expected %s list'%(k, f.instance))
                else:
                    v = self.makeentity(c, v, f)
            if c != v:
                if f.readonly:
                    raise PermissionError("%s is readonly"%(k))
                self.changed.add(ent)
                setattr(ent, k, v)

    def present(self):
        self.changed = set()

        if not self.entity:
            if self.clone:
                clone = self.getentity(self.clone, self.type)
                self.entity = clone.clone()
            else:
                self.entity = self.gettype(self.type)(cluster = self.cluster)
            self.changed.add(self.entity)
            if hasattr(self.entity, 'name'):
                self.entity.name = self.name

        self.setentity(self.entity, self.attrs)

        if self.changed:
            self.result['changed'] = True
            err = self.entity.check()
            if err:
                self.result['failed'] = True
                self.result['msg'] = err
            elif not self.module.check_mode:
                self.entity.commit()

    def run(self):
        self.cluster = pythoncm.cluster.Cluster() # TODO: settings
        self.entity = self.cluster.get_by_name(self.name, self.type)

        self.result = {}
        try:
            getattr(self, self.state)()
        except Exception as e:
            self.result['failed'] = True
            self.result['msg'] = to_native(e)
        if self.entity:
            self.result['entity'] = self.entity.to_dict()
            self.result['name'] = self.entity.resolve_name
            self.result['type'] = self.entity.childType or self.entity.baseType
        return self.result

def main():
    module = AnsibleModule(
        argument_spec=dict(
            name=dict(type='str', required=True),
            type=dict(type='str', required=True, choices=Entity.types if HAS_CM else None),
            state=dict(type='str', default='present', choices=['absent','present']),
            clone=dict(type='str'),
            attrs=dict(type='dict', default={}),
        ),
        supports_check_mode=True,
    )

    if not HAS_CM:
        module.fail_json(msg=missing_required_lib('pythoncm'),
                         exception=CM_IMP_ERR)

    result = Entity(module).run()
    module.exit_json(**result)

if __name__ == '__main__':
    main()
