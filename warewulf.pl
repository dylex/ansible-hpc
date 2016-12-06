#!/usr/bin/perl -w
# WANT_JSON
# Ansible module or dynamic inventory for warewulf.

use strict;
use Data::Dumper;
use JSON;
use List::MoreUtils qw(any);
use Warewulf::Bootstrap;
use Warewulf::DSO::Bootstrap;
use Warewulf::DSO::File;
use Warewulf::DSO::Node;
use Warewulf::DSO::Vnfs;
use Warewulf::DataStore;
use Warewulf::File;
use Warewulf::Ipmi;
use Warewulf::Logger;
use Warewulf::Node;
use Warewulf::Provision;
use Warewulf::Provision::Pxelinux;
use Warewulf::Provision::DhcpFactory;
use Warewulf::Vnfs;

$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy = 1;
sub data_eq($$) {
  my ($a, $b) = @_;
  Dumper($a) eq Dumper($b)
}

sub elem($@) {
  my $e = shift;
  any { $_ eq $e } @_
}

my $JSON = JSON->new->utf8->allow_blessed->convert_blessed->allow_unknown;
$JSON = $JSON->pretty if -t STDOUT;

sub read_json {
  local $/;
  open my $f, '<', @_;
  $JSON->decode(<$f>)
}

sub to_array($) {
  my ($val) = @_;
  $val = [split(' ', $val)]
    unless ref $val eq 'ARRAY';
  wantarray ? @$val : $val
}

sub to_hash($) {
  my ($val) = @_;
  $val = { map { my ($k, $v) = split /=/,$_,2; $k => $v } to_array($val) }
    unless ref $val eq 'HASH';
  wantarray ? %$val : $val
}

sub deobj {
  local $_ = shift;
  if (ref =~ /^Warewulf::/) {
    if ($_->isa('Warewulf::ObjectSet')) {
      my @l = $_->get_list;
      $_ = {};
      for my $e (@l) {
	$_->{$e->get('name')} = deobj($e);
      }
    } elsif ($_->isa('Warewulf::Object')) {
      my %h = $_->get_hash;
      $_ = {};
      while (my ($k, $v) = each %h) {
	$_->{lc $k} = deobj($v);
      }
    }
  } elsif (ref eq 'ARRAY') {
    $_ = [ map { deobj($_) } @$_ ];
  } elsif (ref eq 'HASH') {
    $_ = {%$_};
    for (values %$_) {
      $_ = deobj($_);
    }
  }
  $_
}

sub prop {
  my ($obj, $prop, $args, $check) = @_;
  my $val = $args->{$prop};
  my $cur = $obj->$prop;
  return if data_eq($cur, $val);
  return $check if $check;
  $obj->$prop($val) || JSON::true
}

sub prop_list {
  my ($obj, $prop, $args, $check) = @_;
  my @val = to_array($args->{$prop})
    or die "$prop requires array value\n";
  my @cur = $obj->$prop;
  return if data_eq(\@cur, \@val);
  return $check if $check;
  $obj->$prop(@val) || JSON::true
}

sub prop_adddel {
  my ($obj, $prop, $args, $check) = @_;
  my @val = to_array($args->{$prop});
  my ($base, $addrm) = $prop =~ /^(.*)(add|del)$/
    or die "invalid prop_addrm: $prop";
  $base .= 's';
  my $sense = $addrm eq 'add';
  my @cur = $obj->$base;
  return unless any { $sense xor elem($_, @cur) } @val;
  return $check if $check;
  $obj->$prop(@val) || JSON::true
}

sub prop_bool {
  my ($obj, $prop, $args, $check) = @_;
  my $val = uc($args->{$prop});
  if (elem($val, '1', 'true', 'yes')) {
    $val = 1;
  } elsif (elem($val, '0', 'false', 'no')) {
    $val = 0;
  } else {
    die "$prop requires boolean value\n";
  }
  my $cur = $obj->$prop;
  return unless $val xor $cur;
  return $check if $check;
  $obj->$prop($val) || JSON::true
}

sub prop_bootlocal {
  my ($obj, $prop, $args, $check) = @_;
  my $val = uc($args->{$prop});
  if (elem($val, 'UNDEF', 'FALSE', 'NO', 'N', '0')) {
    $val = 'UNDEF';
  } elsif (not elem($val, 'EXIT', 'NORMAL')) {
    die "$prop requires UNDEF, EXIT, or NORMAL\n";
  }
  my $cur = $obj->$prop;
  $cur = defined $cur ? $cur ? "EXIT" : "NORMAL" : "UNDEF";
  return if $cur eq $val;
  return $check if $check;
  $obj->$prop($val) || JSON::true
}

sub action {
  my ($obj, $prop, $args, $check) = @_;
  return $check if $check;
  $obj->$prop($args->{$prop}) || JSON::true
}

my @NETDEV_PROPS = qw(netrename hwaddr hwprefix ipaddr netmask network gateway fqdn mtu);

sub netdev {
  my ($obj, $prop, $args, $check) = @_;
  my $netdev = $args->{$prop};
  my $netobj = $obj->netdevs($netdev);
  my $changed = 0;
  unless ($netobj) {
    if ($prop eq 'netadd') {
      return $check if $check;
      $obj->netdev_get_add($netdev);
      $changed ++;
    } else {
      return;
    }
  }
  if ($prop eq 'netdel') {
    return $check if $check;
    return $obj->netdel($netdev) || JSON::true;
  }
  for $prop (@NETDEV_PROPS) {
    next unless exists $args->{$prop};
    my $val = $args->{$prop};
    my $cur = $obj->$prop($netdev);
    next if data_eq($cur, $val);
    return $check if $check;
    $obj->$prop($netdev, $args->{$prop});
    $changed ++;
  }
  $changed
}

sub ipmi {
  my ($obj, $prop, $args, $check) = @_;
  return $check if $check;
  system($obj->ipmi_command($args->{$prop}))
    or die "IPMI command failed: $?\n";
}

my %PROPS = (
  node => {
    nodename		=> \&prop,
    cluster 		=> \&prop,
    domain		=> \&prop,
    groups		=> \&prop_list,
    groupadd		=> \&prop_adddel,
    groupdel		=> \&prop_adddel,
    netdev		=> \&netdev,
    netadd		=> \&netdev,
    netdel 		=> \&netdev,
    enabled 		=> \&prop_bool,
    # Provision:
    bootstrapid 	=> \&prop,
    vnfsid		=> \&prop,
    fileids 		=> \&prop_list,
    console 		=> \&prop,
    kargs 		=> \&prop_list,
    pxelinux 		=> \&prop,
    fileidadd		=> \&prop_adddel,
    fileiddel		=> \&prop_adddel,
    master		=> \&prop_list,
    postnetdown		=> \&prop_bool,
    preshell		=> \&prop_bool,
    postshell		=> \&prop_bool,
    selinux		=> \&prop,
    bootlocal		=> \&prop_bootlocal,
    # Impi:
    ipmi_ipaddr		=> \&prop,
    ipmi_netmask	=> \&prop,
    ipmi_username	=> \&prop,
    ipmi_password	=> \&prop,
    ipmi_uid		=> \&prop,
    ipmi_proto		=> \&prop,
    ipmi_autoconfig	=> \&prop_bool,
    ipmi		=> \&ipmi,
  },
  vnfs => {
    name		=> \&prop,
    checksum		=> \&prop,
    chroot		=> \&prop,
    size		=> \&prop,
    vnfs_import		=> \&action,
    vnfs_export		=> \&action,
  },
  bootstrap => {
    name		=> \&prop,
    checksum		=> \&prop,
    size		=> \&prop,
    bootstrap_import	=> \&action,
    bootstrap_export	=> \&action,
    delete_local_bootstrap	=> \&action,
    build_local_bootstrap	=> \&action,
  },
  file => {
    name		=> \&prop,
    mode		=> \&prop,
    checksum		=> \&prop,
    uid			=> \&prop,
    gid			=> \&prop,
    size		=> \&prop,
    path		=> \&prop,
    format		=> \&prop,
    interpreter		=> \&prop,
    origin		=> \&prop,
    sync		=> \&action,
    file_import		=> \&action,
    file_export		=> \&action,
  },
);
my %CLASS = (
  node => 'Warewulf::Node',
  vnfs => 'Warewulf::Vnfs',
  bootstrap => 'Warewulf::Bootstrap',
  file => 'Warewulf::File',
);
my @TYPES = keys %PROPS;

my %DHCP = (
  'update' => 'persist',
  'persist' => 'persist',
  'restart' => 'restart',
);

sub dhcp($$) {
  my ($arg, $check) = @_;
  return unless $arg;
  $arg = $DHCP{$arg};
  die "Unknown dhpc argument: $arg\n"
    unless exists $DHCP{$arg};
  my $op = $DHCP{$arg};
  return $check if $check;
  Warewulf::Provision::DhcpFactory->new->$op() || JSON::true
}

my %PXE = (
  'update' => 'update',
  'delete' => 'delete',
);

sub pxe($$$) {
  my ($arg, $obj, $check) = @_;
  return unless $arg and $obj and $obj->count;
  die "Unknown pxe argument: $arg\n"
    unless exists $PXE{$arg};
  my $op = $PXE{$arg};
  return $check if $check;
  Warewulf::Provision::Pxelinux->new->$op($obj->get_list) || JSON::true
}

my $DS = Warewulf::DataStore->new();

sub match_objects($;$$) {
  my ($match, $field, $action) = @_;
  $field ||= 'name';
  my(@objs, %objs);
  push @objs, \%objs;
  for my $type (@TYPES) {
    next unless exists $match->{$type};
    my $objs = $DS->get_objects($type, $field, to_array($match->{$type}));
    $objs{$type} = $objs;
    push @objs, $objs->get_list;
  }
  @objs
}

sub inventory_objects(;$) {
  my ($host) = (@_, '');
  match_objects { node => $host, vnfs => $host }
}

sub obj_vars($) {
  my ($obj) = @_;
  my %vars;
  $vars{warewulf_type} = $obj->get('_type');
  if ($obj->isa('Warewulf::Node')) {
    $vars{warewulf_domain} = $obj->domain if $obj->domain;
    $vars{warewulf_netdevs} = deobj($obj->netdevs);
  } elsif ($obj->isa('Warewulf::Vnfs')) {
    $vars{ansible_host} = $obj->chroot;
    $vars{ansible_connection} = 'chroot';
  }
  wantarray ? %vars : \%vars
}

sub obj_groups($) {
  my ($obj) = @_;
  my @groups;
  if ($obj->isa('Warewulf::Node')) {
    push @groups, 'warewulf_node';
    push @groups, 'enabled' if $obj->enabled;
    push @groups, $obj->cluster if $obj->cluster;
    push @groups, $obj->groups;
  } elsif ($obj->isa('Warewulf::Vnfs')) {
    push @groups, 'warewulf_vnfs';
  }
  wantarray ? @groups : \@groups
}


my %RES = ();

if      (@ARGV == 1 and $ARGV[0] eq '--list') {
  my ($objs, @objs) = inventory_objects;
  my %vars;
  for my $obj (@objs) {
    my $name = $obj->get('name');
    $vars{$name} = obj_vars($obj);
    for my $group (obj_groups($obj)) {
      push @{$RES{$group}}, $name;
    }
  }
  $RES{_meta} = { hostvars => \%vars };

} elsif (@ARGV == 2 and $ARGV[0] eq '--host') {
  my $host = $ARGV[1];

  my ($objs, @objs) = inventory_objects $host;
  die "No matching host: $host\n" unless @objs;
  die "Multiple matching hosts: $host\n" if $#objs;
  my ($obj) = @objs;
  %RES = host_vars($obj);

} elsif (@ARGV == 1 and -r $ARGV[0]) {
  my $args = read_json(@ARGV);
  $RES{changed} = 0;

  $SIG{__DIE__} = sub {
    die @_ if $^S;
    local $_ = "@_";
    chomp $_;
    $RES{failed} = JSON::true;
    $RES{msg} = $_;
    print $JSON->encode(\%RES);
    exit 0;
  };

  &set_log_target('SYSLOG:ansible-warewulf:' . ($args->{_ansible_syslog_facility} || 'USER'), 'ALL');
  &set_log_target(\&CORE::die, 0, 1);
  set_log_level(($args->{_ansible_verbosity} || 0) + 1);

  my $check = $args->{_ansible_check_mode};

  my $lookup = $args->{lookup};
  my ($objs, @objs) = match_objects($args, $lookup);
  if ($args->{new}) {
    while (my ($type, $obj) = each %$objs) {
      next if $obj->count;
      my $o = $CLASS{$type}->new;
      $o->set('_type', $type);
      $o->$lookup($args->{$type});
      push @objs, $o;
    }
  }

  for my $obj (@objs) {
    my $type = $obj->get('_type');
    my $props = $PROPS{$type};
    my $changed = 0;
    for my $prop (keys %$props) {
      next unless exists $args->{$prop};
      next unless $props->{$prop}->($obj, $prop, $args, $check);
      $changed++;
      last if $check;
    }
    if (exists $args->{set}) {
      my %set = to_hash($args->{set});
      while (my ($key, $val) = each %set) {
	my $cur = $obj->get($key);
	next if data_eq($cur, $val);
	$obj->set($key, $val) unless $check;
	$changed++;
	last if $check;
      }
    }
    ($RES{$type} //= {})->{$obj->get('name')} = deobj($obj)
      if $args->{get};
    if ($changed) {
      $RES{changed} += $changed;
      last if $check;
      $DS->persist($obj);
    }
  }

  if ($args->{delete}) {
    for my $obj (values %$objs) {
      my $n = $obj->count;
      next unless $n;
      $RES{changed} += $n;
      last if $check;
      $DS->del_object($obj);
    }
  }

  $RES{changed}++ if dhcp($args->{dhcp}, $check);
  $RES{changed}++ if pxe($args->{pxe}, $objs->{node}, $check);

} else {
  die "$0: ansible module or dynamic inventory\n"
}

print $JSON->encode(\%RES);
exit 0;
