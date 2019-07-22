#!/bin/perl

$eg_file = $ARGV[0];
chomp $eg_file;

$i=0;
$title ="";

open(F, "$eg_file") || die "can't open $eg_file\n";
while(<F>) 
{ 
    my($line) = $_;
    chomp($line);
    if ($i ==0) {
      $title .= $line . "\n";
    } 
    elsif ($line =~ /^<Entrezgene\-Set>/ || $line =~ /^<\/Entrezgene\-Set>/) {
    }
    elsif ($line =~ /\s*<Entrezgene>/) {
	print "$title" . $line . "\n";
    }
    elsif ($line =~ /\s*<\/Entrezgene>/) {
      print "$line\n\n";
    }
    else {
      unless ($i == 1) {
	print "$line\n";
      }
    }
    $i++;
  }
close(F);
