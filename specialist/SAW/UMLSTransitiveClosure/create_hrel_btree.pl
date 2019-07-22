#!/usr/local/bin/perl

# create a BTree file of pairs of hierarchically related concepts
# <CUI descendant><CUI ancestor>
# in 6-byte binary form

# author :			OB
# created :			03OCT2002

# input:  <CUI descendant><CUI ancestor> (no delimiter)
# output: 6-byte binary code (3 bytes per CUI)


# author :			OB
# created :			03OCT2002

# changes
# 03OCT2002 -- OB
# - initial version
# 04SEP2003 -- OB
# - updated file name to 2003
# - changed Hrel to UMLS::Hrel
# 02JUN2009 -- OB
# - added -bt argument


use strict;
use warnings;

use lib "./lib";
use UMLS::Hrel;
use Time::HiRes qw(gettimeofday);
use BerkeleyDB ; # for DB_CREATE
use Getopt::Long;

# ---------- usage ----------

my $usage = qq{
usage: $0
  -bt  <btree file>
 [-v]
 [-h]
};

# ---------- options ----------

my @options = qw(bt=s h! v!);
my %opt = ();
GetOptions(\%opt, @options);

die "$usage\n" if $opt{h};

my $data_file = $opt{bt};

my $show_details = $opt{v};

# ------------------------------

# time keeper
my $Start_time = gettimeofday;
my $Old_time = $Start_time;
my $Nb_time = 0;

print "data file = $data_file\n";
# initialize BTree file
#unlink $data_file;
UMLS::Hrel::select_hrel_for_query($data_file, 512, DB_CREATE);

while (defined(my $pair = <>)) {
	chomp $pair;
	my ($cui1, $cui2) = unpack('A8 A8', $pair);
	my $key = UMLS::Hrel::insert_hrel($cui1, $cui2);
	warn sprintf "%s\n", timestamp("Lines imported: $.")
	  if $. % 10000 == 0;
}


# ----------

sub timestamp {
	my $where = shift;

	my $current_time = gettimeofday;
	my $time = localtime(time);
	$Nb_time++;
	my $timestamp = sprintf "%s [%05.3f] [%05.3f] [%05.3f] %s",
	  (
	   $time,
	   $current_time - $Old_time,
	   $current_time - $Start_time,
	   ($current_time - $Start_time) / $Nb_time,
	   $where,
	  );
	$Old_time = $current_time;

	return $timestamp;
}
