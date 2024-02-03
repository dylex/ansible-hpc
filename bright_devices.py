#!/cm/local/apps/python3/bin/python3
# -*- coding: utf-8 -*-

from __future__ import absolute_import, division, print_function
__metaclass__ = type

ANSIBLE_METADATA = {
    'metadata_version': '1.0',
    'status': ['preview'],
    'supported_by': 'community'
}

DOCUMENTATION = """
---
module: bright_devices
short_description: Bright cm device selection
description:
  - Lookup Bright Cluster Management devices
notes:
  - This is an interface to bright DeviceSelection.  It could probably be generalized to other entity types.
  - Any of the arguments can take a comma-separated list
  - If mode is intersection and there are no selectors, all devices are returned
  - You will likely have to set C(ansible_python_interpreter=/cm/local/apps/python3/bin/python3) if running this on a bright node.
author:
  - Dylan Simon (@dylex)
requirements:
  - pythoncm
options:
  mode:
    choices: [ union, intersection ]
    default: union
    description: How to combine multiple selectors (lists of items are always unioned)
  only_nodes:
    type: bool
    default: false
    description: Only include nodes (rather than all devices)
  full:
    type: bool
    default: false
    description: Include full device entities, along with names and keys
  category:
    description: select nodes which use the specified category
    type: str
  nodegroup:
    description: select nodes which are contained in the specified node group
    type: str
  configuration_overlay:
    description: select nodes which are contained in the specified configuration overlay
    type: str
  name:
    description: select nodes by name or text range
    type: str
  softwareimage:
    description: select nodes which use the specified software image
    type: str
"""

EXAMPLES = """
- bright_devices:
    mode: intersection
    category: worker
    name: worker[10-20],worker[50-90]
    ansible_python_interpreter: /cm/local/apps/python3/bin/python3
  register: devices
"""


RETURN = """
names:
  description: list of matching device names, sorted by name
  type: list
  returned: always
keys:
  description: list of matching device keys, sorted by name
  type: list
  returned: always
devices:
  description: list of matching device entities, sorted by name
  type: list
  returned: when full
"""

import traceback

from ansible.module_utils.basic import AnsibleModule, missing_required_lib
from ansible.module_utils._text import to_native

CM_IMP_ERR = None
try:
    import pythoncm.cluster
    from pythoncm.device_selection import DeviceSelection

    HAS_CM = True
except ImportError:
    CM_IMP_ERR = traceback.format_exc()
    HAS_CM = False

def add_softwareimage(devices, name):
    img = self.cluster.get_by_name(name, 'SoftwareImage')
    if not img:
        raise KeyError("SoftwareImage %s not found"%(name))
    devices.add_devices(img.nodes)

def add_names(devices, name):
    devices.add_devices(pythoncm.namerange.expand.Expand.expand(name), True)

def main():
    module = AnsibleModule(
        argument_spec=dict(
            mode=dict(type='str', default='union', choices=['union','intersection']),
            only_nodes=dict(type='bool', default=False),
            full=dict(type='bool', default=False),
            category=dict(type='str'),
            nodegroup=dict(type='str'),
            configuration_overlay=dict(type='str'),
            name=dict(type='str'),
            softwareimage=dict(type='str'),
        ),
        supports_check_mode=True,
    )

    if not HAS_CM:
        module.fail_json(msg=missing_required_lib('pythoncm'),
                         exception=CM_IMP_ERR)

    cluster = pythoncm.cluster.Cluster() # TODO: settings
    intersect = module.params['mode'] == 'intersection'
    only_nodes = module.params['only_nodes']
    devices = None

    types = {
          'category': DeviceSelection.add_category
        , 'nodegroup': DeviceSelection.add_nodegroup
        , 'configuration_overlay': DeviceSelection.add_configuration_overlay
        , 'name': add_names #DeviceSelection.add_devices_in_text_range
        , 'softwareimage': add_softwareimage
        }

    for p, f in types.items():
        v = module.params[p]
        if not v: continue
        d = DeviceSelection(cluster, only_nodes=only_nodes)
        for n in v.split(','):
            f(d, n)
        if not devices:
            devices = d
        elif intersect:
            devices = devices.intersection(d)
        else:
            devices = devices.union(d)

    if not devices:
        devices = DeviceSelection(cluster)
        if intersect:
            devices.add_devices(cluster.get_by_type(pythoncm.entity.Node if only_nodes else pythoncm.entity.Device))

    devices = devices.get_sorted_by_name()
    result = {
            'names': [d.resolve_name for d in devices]
          , 'keys': [d.uniqueKey for d in devices]
          }
    if module.params['full']:
        result['devices'] = [d.to_dict() for d in devices]
    module.exit_json(**result)

if __name__ == '__main__':
    main()
