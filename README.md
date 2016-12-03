# Ansible dynamic inventory for Warewulf

Gets information from "wwsh node print" and "wwsh vnfs list".

Generates the following groups:

* warewulf_node: all nodes
* enabled: enabled nodes
* one per cluster
* one per group
* warewulf_vnfs: all VNFS images (which use chroot)

Includes the following host variables:

* warewulf_id
* warewulf_domain
* warewulf_netdevs: one key per interface, each with all defined values (keys in lower case)
* warewulf_vnfs_size: for VNFS images
