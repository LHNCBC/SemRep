#!/bin/perl
$gsfile = shift @ARGV;
chomp($gsfile);
$mmfile = shift @ARGV;
chomp($mmfile);
$srdef = shift @ARGV;
chomp($srdef);


$/ = "";
open(FF, "$mmfile") || die "can;t open $mmfile.\n";
while(<FF>) {
  my($line) = $_; chomp($line);
  if ($line =~ /Phrase: \"(.+?)\.\"/) {
    $phr = $1;
    @els = split(/\n/,$line);
    $map=0;
    for $el (@els) {
      if ($el =~ /Meta Mapping/) {
	$map=1; next;
      } elsif ($el =~ /<<< Mappings/) {
       $map=0;
      }
      if ($map == 1) {
	if ($el =~ /(\d+) (C\d{7}):(.+?) \[(.+?)\]\s*$/) {
	  $cc = $3;
           $cui = $2;
           $semtype = $4;
         if ($cc =~ /\((.+?)\)/) {
           $cc1 = $1;
         } else {
           $cc1 = $cc;
         }
#	  push @{$mm{$phr}}, $cc1;
         $mm{$phr} = $cui . "|" . $cc1 . "|" . $semtype;
       }
      }
    }
     } 
}
close(FF);

$/="\n";
open(SRDEF, "$srdef") || die "can't open $srdef.\n";
%srs = ();
while (<SRDEF>) {
    my($line)= $_; chomp($line);
    @els = split(/\|/,$line);
    $srs{$els[2]} = $els[8];
}

#foreach $p (keys %mm) {
#print "MM: $p | $mm{$p}\n";
#}

$/="\n";
open(F,"$gsfile") || die "can't open $gsfile.\n";
$prev = "";
while(<F>) {
  my ($line) = $_;
  chomp($line);
  next if ($line =~ /^$/);
  if ($line =~ /(\d+)\.(ab|ti)\.(\d+) (.+)/) {
    $pmid = $1;
    $sentid = $1 . "." . $2 . "." . $3;
    $senttext = $4; 
    unless ($pmid eq $prev) {
         print "\n----- Citation $pmid -----\n";
    }
    print "$line\n\n";
  } else {
    @els = split(/\|/,$line);
    $subj = $els[0];
    $obj = $els[2];
    $subjstr = "";
    $objstr = "";
    if (exists $mm{$subj}) {
      $subj_str = $mm{$subj};
    } elsif ($subj =~ /,/) {
      @subjs = split(/, /, $subj);
      $newsubj = "";
      $subjc = (scalar @subjs);
      for ($j=$subjc-1; $j>=0;$j--) {
	$newsubj .= $subjs[$j] . " " ;
      } 
      $newsubj =~ s/\s*$//g;
#      print "NEWSUBJ: $newsubj\n";
      if (exists $mm{$newsubj}) {
	$subj_str = $mm{$newsubj};
      } else { print "Cannot find $subj.\n";}
    }
    if (exists $mm{$obj}) {
      $obj_str = $mm{$obj};
    } elsif ($obj =~ /,/) {
      @objs = split(/,\s/, $obj);
      $newobj = "";
      $objc = (scalar @objs);
      for ($j=$objc-1; $j>=0;$j--) {
	$newobj .= $objs[$j] . " ";
      } 
      $newobj =~ s/\s*$//g;
      if (exists $mm{$newobj}) {
	$obj_str = $mm{$newobj};
      } else { print "Cannot find $obj.\n";}
    }
    unless ($subj_str eq "" || $obj_str eq "") {
      @objs = split(/\|/,$obj_str);
      @subjs = split(/\|/,$subj_str);
      $objsem = $objs[2];
      $subjsem = $subjs[2];
      $objsem_upd = updateSem($objsem);
      $subjsem_upd = updateSem($subjsem);
      $firstsubjsem = $subjsem_upd; $firstsubjsem =~ s/,(.+)//g;
      $firstobjsem = $objsem_upd; $firstobjsem =~ s/,(.+)//g;
      $predication_str = $sentid . "|relation|" . $subjs[0] . "|" . $subjs[1] . "|" . $subjsem_upd . "|" .$firstsubjsem . "|||".  $els[1] . "|" . $objs[0]. "|" . $objs[1]. "|" . $objsem_upd . "|" . $firstobjsem . "||\n";
#      foreach $st (keys %srs) {
#	if ($predication_str =~ /($st)/) {
#	  $abbr = $srs{$st};
#	  $predication_str =~ s/$st/$abbr/g;
#	}
#      }
#      $predication_str =~ s/Patients or DISbled grup/podg/g;
#      $predication_str =~ s/Age grup/aggp/g;
      print $predication_str;
    } else {
      print "Cannot update predication $line.:: SUBJ: $subj_str OBJ: $obj_str\n";
    }
  }
}
close(F); 

sub updateSem {
  my($sem) = @_;
#print "SEM: $sem\n";
  @sems = split(/,/,$sem);
  @finsems = ();
  $semstr = "";
  $i = 0;
  for $s (@sems) {
    if ($i ==0) {
      $semstr = $s;
    }
    elsif ($s =~ /^\s/) {
      $semstr .= "," . $s;
    }
    else {
#print "SEMSTR: $semstr\n";
      push @finsems, $srs{$semstr};
      $semstr = $s;
    }
    $i++;
  }
  unless ($semstr eq "" ) {push @finsems, $srs{$semstr};}
  $finsemstr = join(',',@finsems);
#print "FIN: $finsemstr\n";
  return $finsemstr;
}
