#!/bin/perl
$file = shift @ARGV;
chomp($file);

open(F,"$file") || die "can't open $file.\n";
$prev = "";
while(<F>) {
  my ($line) = $_;
  chomp($line);
  next if ($line =~ /^$/);
  @els = split(/\|/,$line);
  if ($els[5] eq "text") {
    $pmid = $els[1];
    unless ($pmid eq $prev) {
      print "\n----- Citation $pmid -----";
    }
    print "\n" . $pmid . "." . $els[3] . "." . $els[4] . " " . $els[6] . "\n\n";
    $prev = $pmid;
  } elsif ($els[5] eq "relation") {
    $pmid = $els[1];
    $neg =  $els[23];
    if ($neg eq "") {
        $predicate = $els[22];
      } else {
    	$predicate = "NEG_" . $els[22];
    }
    print $pmid . "." . $els[3] . "." . $els[4] . "|relation|" . 
	  $els[8] . "|" . $els[9] . "|" . $els[10] . "|" . $els[11] . "|" . $els[12] . "|" . $els[13] . 
	  "|" . $predicate . "|" .  $els[28] . "|" . $els[29] . "|" . $els[30] . "|" . $els[31] . "|" . $els[32] . "|" . $els[33]. "\n";  
  }
}
close(F); 
