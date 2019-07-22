#!/usr/bin/env perl

# read pairs of hierarchically related concepts
# from BTree file and test if pairs of CUIs exist in the BTree file

# OB -- 02JUN2009

# input:  <CUI descendant><CUI ancestor> (no delimiter)

# author :			OB
# created :			03OCT2002

# changes
# 02JUN2009 -- OB
# - initial version

use strict;
use warnings;

use lib "./lib";
use UMLS::Hrel;
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

# initialize BTree file
UMLS::Hrel::select_hrel_for_query($data_file);

while (defined(my $pair = <>)) {
	chomp $pair;
	my ($cui1, $cui2) = unpack('A8 A8', $pair);
	my $key = UMLS::Hrel::query_hrel($cui1, $cui2);

	my $hrel1 = UMLS::Hrel::query_hrel($cui1, $cui2);
	my $hrel2 = UMLS::Hrel::query_hrel($cui2, $cui1);
	my $res = sprintf "{%8d} $cui1, $cui2 => %s %s", $., ($hrel1 ? 'yes' : 'no'), ($hrel2 ? 'yes' : 'no');

	printf "%s\n",$res;
}

