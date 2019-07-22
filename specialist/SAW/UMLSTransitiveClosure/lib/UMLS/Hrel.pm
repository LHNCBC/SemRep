# UMLS::Hrel.pm# package

# class : hierarchically related concepts

# ** properties : none

# ** methods (public) : 
# - (C) - select_hrel_for_query
#	initialize the BTree file
#	* [arg1] : path to BTree file
#	* [arg2] : cache size in Mb
#	return : none
# - (C) - query_hrel
#	lookup for hierarchical relationship between CUI1 and CUI2
#	* arg1 : CUI1
#	* arg2 : CUI2
#	return : true if there exists a hierarchical relationship

# ** class variables (public) : none


# author :			OB
# created :			03OCT2002
# $Id: Hrel.pm,v 1.2 2003/12/10 20:15:27 zeng Exp $

# changes
# 03OCT2002 -- OB (version 1.00)
# - initial version
# 20NOV2002 -- OB (version 1.01)
# - cleaned-up the code + documentation
# 03SEP2003 -- OB (version 1.01)
# - moved to UMLS::Hrel


package UMLS::Hrel;

# strict Perl
use strict;

# Use Linux berkeley DB 3.0.55 version
# use lib "/nfsvol/crfiler-skr/UMLS/bin/BerkeleyDB";
# use lib "/nfsvol/nls/tools/berkeley_db/Linux-i686/db-4.8.24/lib";
# general libraries
#use lib "/rhome/halil/perllib_4.8.24";
use Carp;
use BerkeleyDB ;
# use lib "/home/shindongwoo/perlmodule/berkeleyDB-3.0.55/lib/perl5/site_perl" ;
# use "/home/shindongwoo/perlmodule/berkeleyDB-3.0.55";
use Time::HiRes qw(gettimeofday);


# ------------------------------------------------------------------------------
#	class public variables
# ------------------------------------------------------------------------------

use vars qw($VERBOSE $VERSION);
$VERBOSE = 0;
$VERSION = '';


# ------------------------------------------------------------------------------
#	class private variables
# ------------------------------------------------------------------------------


my $PACKAGE_NAME = 'UMLS::Hrel';

my %PROPERTIES = (
);

# default cache size
my $DEFAULT_CACHE_SIZE_MB = 100;

# default Hrel file
my $DEFAULT_HREL_FILE = '/aux6/partition-hrel-UMLS_2002';

# default IO mode
my $DEFAULT_IO_MODE = DB_RDONLY;

# Hash to be tied to the BTree file
my %Hrel = ();

# true after the BTree has been initialized
my $Db = undef;


# ------------------------------------------------------------------------------
#	class destructor
# ------------------------------------------------------------------------------

END {
	warn "[destroy class $PACKAGE_NAME]\n" if $VERBOSE;
}


# ------------------------------------------------------------------------------
#	class public methods (class constructor)
# ------------------------------------------------------------------------------

#	new : see super class UNIV_OB


# ------------------------------------------------------------------------------
#	class public methods (other methods)
# ------------------------------------------------------------------------------

#
#	initialize the BTree file
#
sub select_hrel_for_query {
	my $db_file = shift;
	my $cache_size_mb = shift;
	my $io_mode = shift;

	$db_file ||= $DEFAULT_HREL_FILE;
	$io_mode ||= $DEFAULT_IO_MODE;

	my $cache_size = 1024 * 1024 * (defined $cache_size_mb ? $cache_size_mb : $DEFAULT_CACHE_SIZE_MB);

    $Db = new BerkeleyDB::Btree
	  -Filename   => $db_file,
	  -Cachesize  => $cache_size,
	  -Flags  => $io_mode
        or die "Cannot open file $db_file: $! $BerkeleyDB::Error\n" ;

	my $io_mode_txt = 'OTHER';
	$io_mode_txt = 'DB_RDONLY' if $io_mode == DB_RDONLY;
	$io_mode_txt = 'DB_CREATE' if $io_mode == DB_CREATE;
	warn "selected Hrel file: $db_file (IO mode: $io_mode_txt, Cache size: $cache_size)\n";
}

#
#	lookup for hierarchical relationship between CUI1 and CUI2
#
sub query_hrel {
	my $cui1 = shift;
	my $cui2 = shift;

	select_hrel_for_query() unless defined $Db;

	# encode the CUI pair into a binary code
	my $key = cui_pair_to_bin($cui1, $cui2);

	my $value = undef;
	$Db->db_get($key, $value);

	return defined $value;
}

#
#	insert hierarchical relationship between CUI1 and CUI2
#
sub insert_hrel {
	my $cui1 = shift;
	my $cui2 = shift;

	select_hrel_for_query() unless defined $Db;

	# encode the CUI pair into a binary code
	my $key = cui_pair_to_bin($cui1, $cui2);

    $Db->db_put($key, '');
}

#
#	display all hierarchical relationships
#
sub display_all {
	browse_all('display');
}

#
#	count all hierarchical relationships
#
sub count_all {
	return 	browse_all('count');
}


# ------------------------------------------------------------------------------
#	instance public methods (accessor methods)
# ------------------------------------------------------------------------------

# none


# ------------------------------------------------------------------------------
#	instance public methods (instance destructor)
# ------------------------------------------------------------------------------

sub DESTROY {
	my $self = shift;

	warn "[destroy $PACKAGE_NAME instance: $self]\n" if $VERBOSE;
	undef $Db;

	# pass DESTROY message to the base class
	$self->SUPER::DESTROY();
}


# ------------------------------------------------------------------------------
#	instance public methods (other methods)
# ------------------------------------------------------------------------------

# none


# ------------------------------------------------------------------------------
#	instance private methods
# ------------------------------------------------------------------------------

# none


# ------------------------------------------------------------------------------
#	implementation methods
# ------------------------------------------------------------------------------

#
#	transform a pair of CUIs into a 6-byte code
#
sub cui_pair_to_bin {
	my $cui1 = shift;
	my $cui2 = shift;

	# remove leading C: skip 1 grab 7
	my @integers = ();
	foreach my $cui ($cui1, $cui2) {
		push @integers, unpack "x1 A7", $cui;
	}

	# unpack as 8 bytes
	my @bytes = unpack("C8", pack("I2", @integers));
#warn "bin_code_to_cui_pair  bin: @bytes\n";

	# strip the first byte of each integer
	# (i.e., bytes 1 and 4)
	#my $res = pack("C6", @bytes[1 .. 3], @bytes[5 .. 7]);
	my $res = pack("C6", $bytes[2],$bytes[1],$bytes[0],$bytes[6],$bytes[5],$bytes[4]);

	return $res;
}


#
#	transform a 6-byte code into a pair of CUIs
#
sub bin_code_to_cui_pair {
	my $bin = shift;

	# split into 6-bytes
	my @bytes = unpack "C6", $bin;
#warn "bin_code_to_cui_pair  bin1: @bytes\n";

	# pad each 3-byte chunk with a leading null byte
	# transform the 4-byte back into an integer
	# and pad the decimal number as a CUI
	my @cuis = ();
	foreach my $bit_range ([0 .. 2], [3 .. 5]) {
		push @cuis, sprintf "C%07d", unpack("I", pack("C4", 0, @bytes[@$bit_range]));
	}

	return @cuis;
}

#
#	browse all hierarchical relationships
#
sub browse_all {
	my $type = shift;

	die "Incorrect browse type: $type\n"
	  unless $type =~ /^(display|count)$/o;

	select_hrel_for_query() unless defined $Db;

	my $k = '';
	my $v = '';
	my $nb_hrels = 0;
    my $cursor = $Db->db_cursor() ;
    while ($cursor->c_get($k, $v, DB_NEXT) == 0) {
		printf "%s, %s\n", bin_code_to_cui_pair($k)
		  if $type eq 'display';
		$nb_hrels++;
	}

	return $nb_hrels;
}


# ----------------------------------------------------


1; # don't remove this line
