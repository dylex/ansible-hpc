# Ansible module and inventory for Warewulf systems manager

The perl script `warewulf.pl` can be used an an ansible module as well as a dynamic inventory.
To use it, pass it to ansible's `-M` (library) and/or `-i` (inventory) arguments, or place it in the corresponding directories.

Example:

```
- name: boot all nodes from the local disk
  hosts: warewulf_node
  local_action: warewulf node={{inventory_hostname}} bootlocal=EXIT
```

This is equivalent to (though rather slower than) `wwsh provision set --bootlocal=EXIT`.

Requirements:

* [Warewulf](http://warewulf.lbl.gov/trac), with properly configured and populated database
* Run ansible (or, in the case of the module, remotely) on the Warewulf host
* Perl modules: JSON, List::MoreUtils

## Dynamic inventory

When used as a dynamic inventory script (i.e., called with `--list` or `--host HOST`), `warewulf.pl` generates a list of hosts based on Warewulf's node database and vnfs list.

It generates the following host groups:

* warewulf_node: all nodes
* enabled: enabled nodes
* one per warewulf cluster
* one per warewulf group
* warewulf_vnfs: all VNFS images (using chroot)

It provides the following host variables:

* warewulf_domain
* warewulf_netdevs: one key per interface, each with all defined values

### Alternate Haskell version

There is also a Haskell application that provides equivalent dynamic inventory functionality.
It gets information from "wwsh node print" and "wwsh vnfs list", but otherwise behaves similarly to the perl version.
There is probably no reason to use it.

## Module

As a module, `warewulf.pl` allows various interactions with the warewulf database.
You must pass one of the search arguments (`node`, `vnfs`, `bootstrap`, `file`) to specify which objects to operate on.
The argument is a name or list of name patterns by default (but you can search on other fields using `lookup=FIELD`.
You may then additionally specify parameters to set values or take actions.
Generally this corresponds to the `wwsh` interface.

* node=ITEM
   * nodename=STR
   * cluster=STR
   * domain=STR
   * groups=LIST
   * groupadd=LIST
   * groupdel=LIST
   * netdev=STR, netadd=STR, netdel=STR
      * netrename=STR
      * hwaddr=STR
      * hwprefix=STR
      * ipaddr=STR
      * netmask=STR
      * network=STR
      * gateway=STR
      * fqdn=STR
      * mtu=STR
   * enabled=BOOL
   * bootstrapid=STR
   * vnfsid=STR
   * fileids=LIST
   * fileidadd=LIST
   * fileiddel=LIST
   * console=STR
   * kargs=LIST
   * pxelinux=STR
   * master=LIST
   * postnetdown=BOOL
   * preshell=BOOL
   * postshell=BOOL
   * selinux=DISABLED|ENABLED|ENFORCED
   * bootlocal=UNDEF|NORMAL|EXIT
   * ipmi_ipaddr=STR
   * ipmi_netmask=STR
   * ipmi_username=STR
   * ipmi_password=STR
   * ipmi_uid=STR
   * ipmi_proto=STR
   * ipmi_autoconfig=STR
   * ipmi=COMMAND
* vnfs
   * name=STR
   * checksum=STR
   * chroot=STR
   * size=STR
   * vnfs_import=FILE
   * vnfs_export=FILE
* bootstrap
   * name=STR
   * checksum=STR
   * size=STR
   * bootstrap_import=FILE
   * bootstrap_export=PATH
   * delete_local_bootstrap=1
   * build_local_bootstrap=1
* file
   * name=STR
   * mode=STR
   * checksum=STR
   * uid=STR
   * gid=STR
   * size=STR
   * path=STR
   * format=STR
   * interpreter=STR
   * origin=STR
   * sync=1
   * file_import=FILE
   * file_export=FILE
* dhcp=update|restart
* pxe=update|delete

Additionally, you can specify `new=1` to create the object if it none exists, or `delete=1` to delete matching objects.
Finally, if you specify `get=1` the final values of all matching objects will be included in the result, with all properties.
For example, `warewulf node=node01 get=1` may return:

```
{
   "changed" : 0,
   "node" : {
      "node01" : {
         "_id" : "1",
         "_type" : "node",
         "nodename" : "node01",
         "name" : ["node01"],
         "bootstrapid" : "2",
         "vnfsid" : "3"
         "fileids" : ["4", "5"],
         "netdevs" : {
            "eth0" : {
               "name" : "eth0",
               "netmask" : "255.255.255.0",
               "ipaddr" : "10.0.0.1"
            }
         },
         "_ipaddr" : ["10.0.0.1"],
         "bootlocal" : 0,
         "bootloader" : "sda",
         "diskpartition" : "sda",
         "diskformat" : ["sda1", "sda2"],
         "filesystems" : [
            "mountpoint=/:dev=sda1:type=ext4:size=fill",
            "dev=sda2:type=swap:size=2048"
         ]
      }
   }
}
```

Since this module expects to run on the Warewulf server, often it should be used with `local_action` delegation.
