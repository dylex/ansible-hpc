# Ansible dynamic inventory for Warewulf

Gets information from "wwsh node print".

Generates the following groups:

* warewulf: all nodes
* enabled: enabled nodes
* one per cluster
* one per group

Includes the following host variables:

* warewulf_id
* warewulf_domain
* warewulf_netdevs: one key per interface, each with all defined values (keys in lower case)
