#!/usr/bin/perl 
#
# Perl script to compare 2 SemRep runs or to evaluate a run against a gold standard
# The runs must be complete and hopefully error-free (particularly of Scheduler errors)
#
# Halil Kilicoglu, 01/02/2009
#
#
# To call: perl profiler.pl <eval|comp> <semrep_output_file1>:<D|S> <$semrep_output_file2>:<D|S>
#
# Example:
#   perl profiler.pl eval cits1.Usem.v2:D cits1.Usem.v3:S
#
# <eval|comp>: whether we are EVALuating a SemRep output file against a gold standard or 
#                COMParing two SemRep output files, the main mechanism is the same but the output differs
# <semrep_output_file1>:<D|S> : the first SemRep output file and its output type 
#          (D: full-fielded output, S: simple output)         
# <semrep_output_file2>:<D|S> : the second SemRep output file and its output type 
#          (D: full-fielded output, S: simple output), with <eval> this argument is interpreted
#          as being the gold standard file     
# Note that all data are specified relative to current directory.

#use strict;
use File::Copy; 

# Get arguments from $argc, $argv
unless ((scalar @ARGV) == 3) {
	die "Usage: profiler.pl <eval:comp> <semrep_file1>:<output_type(D:S)> <semrep_file2>:<output_type(D:S)>\n";
}

my ($process_type) = shift @ARGV;
chomp($process_type);
my ($semrep1_type_in) = shift @ARGV;
chomp($semrep1_type);
my ($semrep1,$semrep1_type) = split(/:/,$semrep1_type_in);
my ($semrep2_type_in) = shift @ARGV;
chomp($semrep2_type_in);
my ($semrep2, $semrep2_type);
if ($process_type eq "eval" && $semrep2_type_in !~ /:/) {
    $semrep2 = $semrep2_type_in; $semrep2_type = "G";
} else { 
    ($semrep2,$semrep2_type) = split(/:/,$semrep2_type_in);
}

if ($semrep1 eq $semrep2) {
	die("profiler: SemRep output files must be different. \n");
}

# find the location of JavaScript files
my ($program_name) = $0;
$program_name      =~ /(.+)(\/|\\)/;
my ($js_dir)  = $1 . $2 . "JS_files";
print "JS DIR: $js_dir\n";

# global hash variables
# hash of semantic types in two files
my (%SemTypesH) = ();
# hash of predicates in two files
my (%PredicatesH) = ();
# hash of predicates-arguments in two files
my (%PredicateArgumentsH) = ();

print "Reading SemRep output files...\n";
my ($SEMREP1_DATA) = LoadSemrepData($semrep1,$semrep1_type);
my ($SEMREP2_DATA) = LoadSemrepData($semrep2,$semrep2_type);
print "Creating HTML files for citations ...\n";
CreateHTMLFiles($SEMREP1_DATA, $semrep1);
CreateHTMLFiles($SEMREP2_DATA, $semrep2);
print "Comparing SemRep output files...\n";
my ($DIFFS) = FindDiffs($SEMREP1_DATA, $SEMREP2_DATA);
print "Creating output HTML files...\n";
CreateSummaryPage($process_type, $SEMREP1_DATA, $SEMREP2_DATA, $DIFFS);
CreateFullPredicationPage($SEMREP1_DATA, $SEMREP2_DATA);
CreateFullSemtypePage($SEMREP1_DATA, $SEMREP2_DATA);
CreateDiffPredicationPage($DIFFS);
CreateDiffSemtypePage($DIFFS);
CopyJSFiles($js_dir);


################################################################
sub LoadSemrepData {
   my ( $SemRepOutputFile, $OutputType ) = @_;
   my ( $line, $PREV_PMID, $PMID, $TiOrAb, $SentenceID, $UtteranceID );
   my ( @fields, $SemTypePair );
   my ( $Predicate, $Relation, $SubjectSemType, $ObjectSemType );
   my ( %Citations, %Utterances, %Predications, %Predicates, %Relations );
   my ( %CitationUtterances, %UtterancePredications );
   my ( %PredicatesTotal, %PredicatesRelation, %SubjectSemTypes );
   my ( %SubjectSemTypesTotal, %ObjectSemTypes, %ObjectSemTypesTotal );
   my ( %SEEN ) = ();
   my ( $CitationCount, $UtteranceCount, $PredicationCount);

   my @ALLDATA = ( undef, # This "undef" here is just a placeholder to make counting easier!, 
       \%Citations,             \%Utterances,      , \%CitationUtterances, 
        \%Predications,          \%UtterancePredications,
        \%Predicates,            \%Relations,       \%PredicatesTotal,
         \%PredicatesRelation,    \%SubjectSemTypes, \%SubjectSemTypesTotal,
          \%ObjectSemTypes,        \%ObjectSemTypesTotal);
   open (INFILE, $SemRepOutputFile) || die "can't open $SemRepOutputFile.\n";
   $PREV_PMID = -9999;
   while (<INFILE>) {
     chomp;
     $line = $_;
     @fields = split /\|/, $line;
     # add bogus field to make counting of fields easier!
     unshift @fields, "bogus";
     $scale_line = 0;
     if ($OutputType eq "D") {
       $field_type = $fields[6];
       next if ($field_type ne "relation" && $field_type ne "text" && $field_type ne "entity");
       if ($fields[6] eq "text") {
	 $PMID        = $fields[2];
	 $TiOrAb      = $fields[4];
	 $SentenceID  = $fields[5];
	 $UtteranceID = $PMID . "." . $TiOrAb . "." . $SentenceID;
	 if ((scalar @fields ) == 8) {
	     $SentenceText = $UtteranceID ." " . $fields[7];
	 } elsif ((scalar @fields) == 10) {
	     $SentenceText = $UtteranceID ." " . $fields[9];
	 }
	 print "FILE: " . (scalar @fields) . " " . $SemRepOutputFile . " " . $SentenceText . "\n";
	 if ( $PMID != $PREV_PMID ) {
	   if ($PREV_PMID) {$SEEN{$PREV_PMID} = 1;}
	   $PREV_PMID = $PMID;
	   unless ($SEEN{$PMID} == 1) {
	   # increment the total citation count
	   ++$Citations{$PMID};
	   # initialize %Predications hashes
	   $Predications{$PMID} = 0;
	 }
	 } # if ( $PMID != $PREV_PMID )
       } elsif ($field_type eq "relation") {
	 $SubjectSemType = $fields[12];
	 ++$SemTypesH{$SubjectSemType};
	 $Predicate      = $fields[23];
	 $Neg            = $fields[24];
	 if ($Neg ne "") {
	   $Predicate = "NEG_" . $Predicate;
	 }
	 $ObjectSemType  = $fields[32];
	 ++$SemTypesH{$ObjectSemType};
	 $PredicationText = $UtteranceID . "|relation|" . 
                            $fields[9] . "|" . $fields[10] . "|" . $fields[11] . "|" . $fields[12] . "|" . 
			    $fields[13] . "|" . $fields[14] . "|" . $Predicate . "|" . 
			    $fields[29] . "|" . $fields[30] . "|" . $fields[31] . "|" . $fields[32] . "|" . 
			    $fields[33] . "|" . $fields[34];
       } elsif ($field_type eq "entity" && $fields[8] =~ /\<\<(.+?)\>\>/) {
	 $PredicationText = $UtteranceID . "|relation|scale|" . $1;
	 $Predicate = $1;
	 $scale_line = 1;
       }
       
     } elsif ($OutputType eq "S") {
       $field_type = "";
       unless ($line =~ /^\s*$/ || $line =~ /\*\*\* ERROR/) {
	 if ($line =~ /Citation ([\d\w]+)/) {
	   if ($PMID) { $SEEN{$PMID} = 1;}
	   $PMID = $1;
	   unless ($SEEN{$PMID} == 1) {
	     ++$Citations{$PMID};
	     $Predications{$PMID} = 0;
	   }
	} elsif ($line =~ /\|relation\|scale\|/) {
	   $field_type ="relation";
	   $PredicationText = $line;
	   $Predicate = $fields[4];
	   $scale_line = 1;
	 } elsif ($line =~ /\|relation\|/) {
	   $field_type = "relation";
	   #$PredicationText = $line;
	   $PredicationText = $fields[1] . "|" . $fields[2] . "|" . $fields[3] . "|" . $fields[4] . "|".
			      $fields[5] . "|" . $fields[6] . "|" . $fields[7] . "|" . $fields[8] . "|" . 
			      $fields[9] . "|" . $fields[10] . "|" . $fields[11] . "|" . $fields[12] . "|" .
			      $fields[13] . "|" . $fields[14] . "|" . $fields[15];
	   $SubjectSemType = $fields[6];
	   ++$SemTypesH{$SubjectSemType};
	   $Predicate = $fields[9];
	   $ObjectSemType = $fields[13];
	   ++$SemTypesH{$ObjectSemType};
	 } else {
	   $line =~ /^(.+?)\s/;
	   $UtteranceID = $1;
           $UtteranceID =~ /([\d\w]+)\.(ab|ti)\.(\d+)/;
	   $PMID = $1;
	   $SentenceText = $line; 
	   $field_type = "text";
	 }
       }
     } elsif ($OutputType eq "G") {
     }

     unless ($SEEN{$PMID} == 1 ) {
       if ($field_type eq "text") {
	 ++$Utterances{$UtteranceID};
	 $CitationUtterances{$PMID}{$UtteranceID} = $SentenceText;
       } elsif ($field_type eq "relation" && $scale_line == 0) {
	 ++$UtterancePredications{$PMID}{$UtteranceID}{$PredicationText};
	 $SemTypePair    = $SubjectSemType . " " . $ObjectSemType;
	 $Relation       = $SubjectSemType . "|" . $Predicate . "|" . $ObjectSemType; 
	 # increment the total predication count
	 ++$Predications{$PMID};
	 # increment the Subject SemType count
	 ++$SubjectSemTypes{$PMID}{$SubjectSemType};
	 ++$SubjectSemTypesTotal{$SubjectSemType};
	 # increment the Predicate count
	 ++$Predicates{$PMID}{$Predicate};
	 ++$PredicatesTotal{$Predicate};
	 # ++$PredicatesRelation{$Predicate}{$SemTypePair};
	 ++$PredicatesRelation{$Predicate}{$SemTypePair}{$UtteranceID};
	 #push @{$PredicatesRelation{$Predicate}{$SemTypePair}}, $UtteranceID;
	 ++$PredicatesH{$Predicate};
	 ++$PredicateArgumentsH{$Predicate}{$SemTypePair};

	 # increment the Object SemType count
	 ++$ObjectSemTypes{$PMID}{$ObjectSemType};
	 ++$ObjectSemTypesTotal{$ObjectSemType};
       
	 # increment the Relation count
	 push @{$Relations{$PMID}{$UtteranceID}}, $Relation;
	} elsif ($scale_line == 1) {
	 ++$UtterancePredications{$PMID}{$UtteranceID}{$PredicationText};
	 $Relation       = "|" . $Predicate . "|" ; 
         # increment the total predication count
         ++$Predications{$PMID};
         # increment the Predicate count
         ++$Predicates{$PMID}{$Predicate};
         ++$PredicatesTotal{$Predicate};
         ++$PredicatesH{$Predicate};

         # increment the Relation count
         push @{$Relations{$PMID}{$UtteranceID}}, $Relation;
       }
     }
   } # while (<INFILE>)

   close INFILE;
   return \@ALLDATA;
} # sub load_data_for_summary

#########################################################
sub CreateHTMLFiles {
  my ($ALLDATA, $file) = @_;
   
  my %Citations    = %{$$ALLDATA[1]};
  my %Utterances   = %{$$ALLDATA[3]};
  my %Predications = %{$$ALLDATA[5]};

  $dirname = $semrep1 . "." . $semrep2 . ".DIFF_files";
  mkdir $dirname;
  foreach $pmid (keys %Citations) {
    $filename = $dirname . "/" . $file . "." . $pmid . ".html";
    open(FFF, ">$filename") or die "can't open $filename";
    print FFF "<HTML>\n";
    print FFF "<HEAD>\n";
    print FFF "<TITLE>$file</TITLE>\n";
    print FFF "</HEAD>\n";
    print FFF "<BODY>\n";
    print FFF "<br><br>----- Citation $pmid -----\n";
    foreach $utterance (sort UtteranceSort keys %{$Utterances{$pmid}}) { 
      $sentence = $Utterances{$pmid}{$utterance};
      print FFF "<br><br><a name=\"$utterance\"/>$sentence</a><br>\n";
      foreach $predication (keys %{$Predications{$pmid}{$utterance}}) {
	@pred_elements = split(/\|/, $predication) ;
	$subj = $pred_elements[3];
	$subj_semtypes = $pred_elements[4];
	$subj_semtype = $pred_elements[5];
	$subj_cui = $pred_elements[2];
	$subj_geneid = $pred_elements[6];
	$subj_gene = $pred_elements[7];
	$predicate = $pred_elements[8];
	$obj = $pred_elements[10];
	$obj_semtypes = $pred_elements[11];
	$obj_semtype = $pred_elements[12];
	$obj_cui = $pred_elements[9];
	$obj_geneid = $pred_elements[13];
	$obj_gene = $pred_elements[14];
	$cnt = $Predications{$pmid}{$utterance}{$predication};
	for ($i=0;$i<$cnt;$i++) {
	  print FFF "<br>" . $utterance . "|relation|" . 
	          $subj_cui . "|<font color=\"blue\">" . $subj . "</font>|" . $subj_semtypes . "|<font color=\"green\">" . $subj_semtype . "</font>|" . 
                  $subj_geneid . "|<font color=\"blue\">" . $subj_gene . "</font>|<font color=\"red\">" . $predicate . "</font>|" . 
		  $obj_cui  . "|<font color=\"blue\">" . $obj .  "</font>|" . $obj_semtypes  . "|<font color=\"green\">" . $obj_semtype  . "</font>|" . 
                  $obj_geneid  . "|<font color=\"blue\">" . $obj_gene . "</font>\n";
        }
      }
    }
    close(FFF);
  }
}

##################################################################
sub CreateSummaryPage {
  my($process_type, $SEMREP1_DATA, $SEMREP2_DATA, $DIFFS) = @_;
  my $CitationCount1 = (scalar keys %{$$SEMREP1_DATA[1]});
  my $UtteranceCount1 = (scalar keys %{$$SEMREP1_DATA[2]});
  my %Predications1 = %{$$SEMREP1_DATA[4]};
  my $CitationCount2 = (scalar keys %{$$SEMREP2_DATA[1]});
  my $UtteranceCount2 = (scalar keys %{$$SEMREP2_DATA[2]});
  my %Predications2 = %{$$SEMREP2_DATA[4]};

  my $PredicationCount1 = 0;
  my $PredicationCount2 = 0;
  for $PMID ( keys %Predications1 ) {
    $PredicationCount1 += $Predications1{$PMID};
  } 
  for $PMID ( keys %Predications2 ) {
    $PredicationCount2 += $Predications2{$PMID};
  }
  if ($process_type eq "comp") {
    $filename = $semrep1 . "_" . $semrep2.  "_comp.html";
  } elsif ($process_type eq "eval") {
    $filename = $semrep1 . "_" . $semrep2.  "_eval.html";
  }
  open(FFF, ">$filename") or die "can't open $filename";
  print FFF "<HTML>\n";
  print FFF "<HEAD>\n";
  print FFF "<TITLE>$filename</TITLE>\n";
  print FFF "</HEAD>\n";
  print FFF "<BODY>\n";
  if ($process_type eq "comp") {
    print FFF "<h1>SemRep Output Comparison Results:</h1>\n";
    print FFF "<b>File 1:</b> " . $semrep1 . "\n";
    print FFF "<br><b>File 2:</b> " . $semrep2 . "<br>\n";
  } elsif ($process_type eq "eval") {
    print FFF "<h1>SemRep Evaluation Results:</h1>\n";
    print FFF "<b>Evaluated File: </b>"     . $semrep1 . "\n";
    print FFF "<br><b>Gold Standard File: </b>" . $semrep2 . "<br>\n";
  }
  
  print FFF "<br><b>Statistics:</b><br>\n";
  print FFF "<TABLE BORDER=\"1\"><TR><TD/><TD>Citation Count </TD><TD>Utterance Count </TD><TD>Predication Count </TD></TR>\n";
  print FFF "<TR><TD>" . $semrep1 . "</TD><TD>"    . $CitationCount1    . "</TD><TD>" . $UtteranceCount1    . "</TD><TD>" . $PredicationCount1 . "</TD></TR>\n";
  print FFF "<TR><TD>" . $semrep2 . "</TD><TD>"    . $CitationCount2    . "</TD><TD>" . $UtteranceCount2    . "</TD><TD>" . $PredicationCount2 . "</TD></TR>\n";
  print FFF "</TABLE>\n";
  if ($process_type eq "eval") {
    ($TPs, $FPs, $FNs) = CreateEvaluationFile($DIFFS);

    $precision = $TPs / ($TPs + $FPs);
    $recall = $TPs / ($TPs + $FNs);
    $fscore = (2 * $precision * $recall) / ($precision + $recall);
    $precision_str = sprintf("%5.3f", $precision);
    $recall_str = sprintf("%5.3f", $recall);
    $fscore_str = sprintf("%5.3f", $fscore);
    print FFF "<br><b>Evaluation Summary:</b>\n";
    print FFF "<br><TABLE BORDER=\"1\"><TR><TD>True Positives: </TD><TD> " . $TPs . "</TD></TR>\n";
    print FFF "<TR><TD>False Positives: </TD><TD>" . $FPs . "</TD></TR>\n";
    print FFF "<TR><TD>False Negatives: </TD><TD>" . $FNs . "</TD></TR>\n";
    print FFF "<TR><TD>Precision: </TD><TD>$precision_str</TD></TR>\n";
    print FFF "<TR><TD>Recall: </TD><TD>$recall_str</TD></TR>\n";
    print FFF "<TR><TD>F-Score: </TD><TD>$fscore_str</TD></TR>\n";
    print FFF "</TABLE>\n";

    print FFF "<br><a href=\"" .$semrep1 . "_" . $semrep2 . "_evalres.html\" target=\"_blank\">Full Evaluation Results</a>\n";
  }
    
  print FFF "<br><br><a href=\"" .$semrep1 . "_" . $semrep2 . "_pred.html\" target=\"_blank\">Comparison of Predications</a><br>\n";
  print FFF "<a href=\"" .$semrep1 . "_" . $semrep2 . "_pred_diff.html\" target=\"_blank\">Comparison of Predication Differences</a><br>\n";
  print FFF "<a href=\"" .$semrep1 . "_" . $semrep2 . "_semtype.html\" target=\"_blank\">Comparison of Semantic Types</a><br>\n";
  print FFF "<a href=\"" .$semrep1 . "_" . $semrep2 . "_semtype_diff.html\" target=\"_blank\">Comparison of Semantic Types in Predication Differences</a><br>\n";
  print FFF "</BODY>\n";
  print FFF "</HTML>\n";
  close(FFF);
}

#############################################################################
#Create the main frame of predication comparison page
sub CreateFullPredicationPage {
  my ($SEMREP1_DATA, $SEMREP2_DATA) = @_;
  my ($filename) = $semrep1 . "_" . $semrep2 . "_pred.html";
  my ($filename_left) = $semrep1 . "_" . $semrep2 . "_pleft.html";
  my ($filename_js) = $semrep1 . "_" . $semrep2 . ".js";
  CreateMainFrameHtml($filename, $filename_left, "Comparison of Predications in " . $semrep1 . " and " . $semrep2);
  CreateLeftFrameHtml($filename_left, $filename_js);
  CreateJSFile($filename_js,$SEMREP1_DATA, $SEMREP2_DATA);
}

#############################################################################
sub CreateMainFrameHtml {
  my ($filename, $filename_left, $title) = @_;
  open(FM, ">$filename") or die "can't open $filename";
  print FM "<html>\n";
  print FM "<head>\n";
  print FM "<title>$title</title>\n";
  print FM "<script>function op() \{ //This function is used with folders that do not open pages themselves. See online docs.\} </script>\n";
  print FM "</head>\n";
  print FM "<!-- (Please keep all copyright notices.) This frameset document includes the Treeview script. Script found in: http://www.treeview.net Author: Marcelino Alves Martins -->\n";
  print FM "<FRAMESET cols=\"300,*\">\n"; 
  print FM "<FRAME src=\"" . $filename_left . "\" name=\"treeframe\">\n"; 
  print FM "<FRAMESET rows=\"50%,*\">\n";
  print FM "<FRAME SRC=\"$first_file_semrep1\" name=\"frm1\">\n"; 
  print FM "<FRAME SRC=\"$first_file_semrep2\" name=\"frm2\">\n";   
  print FM "</FRAMESET>\n";
  print FM "</FRAMESET>\n";
  print FM "</HTML>\n";
  close(FM);
}

##############################################################################
# Left frame holds the JS tree. All we need to do is initialize the tree here. Tree content is created separately.
sub CreateLeftFrameHtml {
  my ($filename, $filename_js) = @_;
  open(FL, ">$filename") or die "can't open $filename"; 
  print FL "<!-- (Please keep all copyright notices.) This frameset document includes the Treeview script. Script found at: http://www.treeview.net Author: Marcelino Alves Martins  See demoFramesetLeftFrame.html for instructions specific to the left frame-->\n";
  print FL "<HTML>\n";
  print FL "<head>\n";
  print FL "<style>\n";
  print FL "BODY {background-color: white}\n";
  print FL "TD {font-size: 10pt; font-family: verdana,helvetica; text-decoration: none; white-space:nowrap;}\n";
  print FL "A  {text-decoration: none; color: black}\n";
  print FL "</style>\n";

  print FL "<script src=\"ua.js\"></script>\n";
  print FL "<script src=\"ftiens4.js\"></script>\n";
  print FL "<script src=\"$filename_js\"></script>\n";
  print FL "</head>\n";

  print FL "<body topmargin=16 marginheight=16>\n";
  print FL "<!-- By making any changes to this code you are violating your user agreement. Corporate users or any others that want to remove the link should check the online FAQ for instructions on how to obtain a version without the link -->\n";
  print FL "<!-- Removing this link will make the script stop from working -->\n";
  print FL "<div style=\"position:absolute; top:0; left:0; \"><table border=0><tr><td><font size=-2><a style=\"font-size:7pt;text-decoration:none;color:silver\" href=http://www.treemenu.net/ target=_blank>JavaScript Tree Menu</a></font></td></tr></table></div>\n";
  print FL "<!-- Build the browser's objects and display default view of the tree. -->\n";
  print FL "<script>initializeDocument()</script><noscript>A tree for site navigation will open here if you enable JavaScript in your browser.</noscript>\n";
  print FL "</html>\n"; 

  close(FL);
}

################################################################################
# Create the JS file based on TreeView API.
sub CreateJSFile {
  my($filename, $DATA1, $DATA2) = @_;
  open(FFF, ">$filename") or die "can't open $filename";
  print FFF "USETEXTLINKS = 1\n";  
  print FFF "STARTALLOPEN = 0\n";
  print FFF "ICONPATH = ''\n";
  print FFF "HIGHLIGHT = 1\n";
#  print FFF "PRESERVESTATE = 1\n";
  print FFF "GLOBALTARGET=\"S\"\n"; 

  $predicate_level = 0;
  my (%PredicateCounts1) =  %{$$DATA1[8]};
  my (%PredicateCounts2) =  %{$$DATA2[8]}; 
  my (%PredicateArguments1) = %{$$DATA1[9]};
  my (%PredicateArguments2) = %{$$DATA2[9]};
  print FFF "foldersTree = gFld(\"PREDICATES($semrep1/$semrep2)\", \"\")\n";
  foreach $predicate (sort keys %PredicatesH) {
      if (exists $PredicateCounts1{$predicate}) {$pred_count1 = $PredicateCounts1{$predicate}; } else { $pred_count1 = 0;}
      if (exists $PredicateCounts2{$predicate}) {$pred_count2 = $PredicateCounts2{$predicate}; } else { $pred_count2 = 0;}
      print FFF "predicate" . ++$predicate_level . " = gFld(\"" . $predicate . "(" . $pred_count1 . "/" . $pred_count2 . ")\",\"\")\n";
      %pred_arg_counts1 = %{$PredicateArguments1{$predicate}};
      %pred_arg_counts2 = %{$PredicateArguments2{$predicate}};
      %PredArgumentsH = %{$PredicateArgumentsH{$predicate}};
      $predication_level = 0;
      foreach $pair (sort keys %PredArgumentsH) {
        %pred_arg_hash1 = %{$pred_arg_counts1{$pair}};
        %pred_arg_hash2 = %{$pred_arg_counts2{$pair}};
	if (%pred_arg_hash1) { $pred_arg_count1 = (scalar keys %pred_arg_hash1);} else { $pred_arg_count1 = 0;}
	if (%pred_arg_hash2) { $pred_arg_count2 = (scalar keys %pred_arg_hash2);} else { $pred_arg_count2 = 0;}
	$pair =~ /(.+?) (.+)/;
	$count1 = 0;
	foreach (%pred_arg_hash1) {
	  $count1 += $pred_arg_hash1{$_};
	}
	$count2 = 0;
	foreach (%pred_arg_hash2) {
	  $count2 += $pred_arg_hash2{$_};
	}
	$predication = $1 . "|" . $predicate . "|" . $2;
        print FFF "predication" . $predicate_level . ++$predication_level  . " = " . 
                      "gFld(\"" . $predication . "(" .  $count1 . "/" . $count2 . ")\",\"\")\n";
       print FFF "predication" . $predicate_level . $predication_level . ".addChildren([" ;
        @PairUtterances = @{Union(\%pred_arg_hash1,\%pred_arg_hash2)};
        $j =0;

	foreach $x (sort UtteranceSort @PairUtterances ) {
	    if (exists $pred_arg_hash1{$x}) { $pred_arg_cnt1 = $pred_arg_hash1{$x};} else { $pred_arg_cnt1 = 0;}
	    if (exists $pred_arg_hash2{$x}) { $pred_arg_cnt2 = $pred_arg_hash2{$x};} else { $pred_arg_cnt2 = 0;}
	    $j++;
	    if ($j > 1) {
	      print FFF ",";
	    } 
	    print FFF "[\"". $x . "(" . $pred_arg_cnt1 . "/" . $pred_arg_cnt2 . ")\",  \"javascript:scrollFrame(\\\'" . $x . "\\\')\"]";
	}
	print FFF "])\n";	  
      }
      print FFF "predicate" . $predicate_level . ".addChildren([" ;
      for ($i=1; $i<= $predication_level; $i++) {
         if ($i > 1) {
            print FFF ",";
         }
         print FFF "predication" . $predicate_level . $i;
      }
      print FFF "])\n";
  }
  print FFF "foldersTree.addChildren([" ;
  for ($i=1; $i<= $predicate_level; $i++) {
    if ($i > 1) {
      print FFF ",";
    }
    print FFF "predicate" . $i;
  }
  print FFF "])\n";

  $dirname = $semrep1 . "." . $semrep2 . ".DIFF_files";
  print FFF "\n\nfunction scrollFrame(id) {\n";
  print FFF "var ind = id.indexOf(\".\");\n";
  print FFF "var pmid = id.substring(0,ind);\n";
  print FFF "var semrep1_link = \'" . $dirname. "/" . $semrep1 . ".\' + pmid + \'.html\';\n";
  print FFF "var semrep2_link = \'" . $dirname. "/" . $semrep2 . ".\' + pmid + \'.html\';\n";
  print FFF "top.frames['frm1'].location=semrep1_link + '#' + id;\ntop.frames['frm2'].location=semrep2_link + '#' + id;}\n";
  close (FFF);
}

#########################################################################
# Create the Diff JS file based on TreeView API.
sub CreateJSDiffFile {
  my($filename, $DATA) = @_;
  open(FFF, ">$filename") or die "can't open $filename";
  print FFF "USETEXTLINKS = 1\n";  
  print FFF "STARTALLOPEN = 0\n";
  print FFF "ICONPATH = ''\n";
  print FFF "HIGHLIGHT = 1\n";
#  print FFF "PRESERVESTATE = 1\n";
  print FFF "GLOBALTARGET=\"S\"\n"; 

  $predicate_level = 0;
  my (%PredicateCounts1) = %{$$DATA[5]};
  my (%PredicateCounts2) = %{$$DATA[6]};
  my (%PredicateArguments1) = %{$$DATA[7]};
  my (%PredicateArguments2) = %{$$DATA[8]};
  my (%PredicateArgumentsH) = %{$$DATA[9]};
  print FFF "foldersTree = gFld(\"PREDICATE DIFFS($semrep1/$semrep2)\", \"\")\n";
  foreach $predicate (sort keys %PredicateArgumentsH) {
    if (!exists $PredicateCounts1{$predicate} && !exists $PredicateCounts2{$predicate}) { next;}
    if (exists $PredicateCounts1{$predicate}) {$pred_count1 = $PredicateCounts1{$predicate}; } else { $pred_count1 = 0;}
    if (exists $PredicateCounts2{$predicate}) {$pred_count2 = $PredicateCounts2{$predicate}; } else { $pred_count2 = 0;}
    print FFF "predicate" . ++$predicate_level . " = gFld(\"" . $predicate . "(" . $pred_count1 . "/" . $pred_count2 . ")\",\"\")\n";
    %pred_arg_counts1 = %{$PredicateArguments1{$predicate}};
    %pred_arg_counts2 = %{$PredicateArguments2{$predicate}};
    %PredArgumentsH = %{$PredicateArgumentsH{$predicate}};
    $predication_level = 0;

    foreach $pair (sort keys %PredArgumentsH) {
      %pred_arg_hash1 = %{$pred_arg_counts1{$pair}};
      %pred_arg_hash2 = %{$pred_arg_counts2{$pair}};
      if (%pred_arg_hash1) { $pred_arg_count1 = (scalar keys %pred_arg_hash1);} else { $pred_arg_count1 = 0;}
      if (%pred_arg_hash2) { $pred_arg_count2 = (scalar keys %pred_arg_hash2);} else { $pred_arg_count2 = 0;}
      $pair =~ /(.+?) (.+)/;
      $count1 = 0;
      foreach (%pred_arg_hash1) {
	$count1 += $pred_arg_hash1{$_};
      }
      $count2 = 0;
      foreach (%pred_arg_hash2) {
	$count2 += $pred_arg_hash2{$_};
      }
      $predication = $1 . "|" . $predicate . "|" . $2;
      print FFF "predication" . $predicate_level . ++$predication_level  . " = " . 
	"gFld(\"" . $predication . "(" .  $count1 . "/" . $count2 . ")\",\"\")\n";
      print FFF "predication" . $predicate_level . $predication_level . ".addChildren([" ;
        @PairUtterances = @{Union(\%pred_arg_hash1,\%pred_arg_hash2)};
        $j =0;

	foreach $x (sort UtteranceSort @PairUtterances ) {
	    if (exists $pred_arg_hash1{$x}) { $pred_arg_cnt1 = $pred_arg_hash1{$x};} else { $pred_arg_cnt1 = 0;}
	    if (exists $pred_arg_hash2{$x}) { $pred_arg_cnt2 = $pred_arg_hash2{$x};} else { $pred_arg_cnt2 = 0;}
	    $j++;
	    if ($j > 1) {
	      print FFF ",";
	    } 
	    print FFF "[\"". $x . "(" . $pred_arg_cnt1 . "/" . $pred_arg_cnt2 . ")\",  \"javascript:scrollFrame(\\\'" . $x . "\\\')\"]";
	}
	print FFF "])\n";	  
      }
      print FFF "predicate" . $predicate_level . ".addChildren([" ;
      for ($i=1; $i<= $predication_level; $i++) {
         if ($i > 1) {
            print FFF ",";
         }
         print FFF "predication" . $predicate_level . $i;
      }
      print FFF "])\n";
  }
  print FFF "foldersTree.addChildren([" ;
  for ($i=1; $i<= $predicate_level; $i++) {
    if ($i > 1) {
      print FFF ",";
    }
    print FFF "predicate" . $i;
  }
  print FFF "])\n";

  $dirname = $semrep1 . "." . $semrep2 . ".DIFF_files";
  print FFF "\n\nfunction scrollFrame(id) {\n";
  print FFF "var ind = id.indexOf(\".\");\n";
  print FFF "var pmid = id.substring(0,ind);\n";
  print FFF "var semrep1_link = \'" . $dirname. "/" . $semrep1 . ".\' + pmid + \'.html\';\n";
  print FFF "var semrep2_link = \'" . $dirname. "/" . $semrep2 . ".\' + pmid + \'.html\';\n";
  print FFF "top.frames['frm1'].location=semrep1_link + '#' + id;\ntop.frames['frm2'].location=semrep2_link + '#' + id;}\n";
  close (FFF);
}

#########################################################################
sub CopyJSFiles {
  my ($dir) = @_;
  opendir JSDIR, $dir;
  while ($file = readdir(JSDIR)) {
    unless ($file eq '.' || $file eq '..') {
      unless (-e $file) {
	copy($dir. '/' .$file, $file) or die "copy failed: $!";
      }
    }
  }
}

#########################################################################
sub CreateFullSemtypePage {
  my($DATA1,$DATA2) = @_;
  $filename = $semrep1 . "_" . $semrep2 . "_semtype.html";
  %SubjectSemTypes1 = %{$$DATA1[11]};
  %ObjectSemTypes1  = %{$$DATA1[13]};
  %SubjectSemTypes2 = %{$$DATA2[11]};
  %ObjectSemTypes2  = %{$$DATA2[13]};
  CreateSemtypePage($filename,"Semantic Type Distribution in Predications",  
		    \%SubjectSemTypes1, \%ObjectSemTypes1, \%SubjectSemTypes2, \%ObjectSemTypes2);
}

#########################################################################
sub CreateDiffSemtypePage {
  my ($DATA) = @_;
  $filename = $semrep1 . "_" . $semrep2 . "_semtype_diff.html";
  %SubjectSemTypes1 = %{$$DATA[10]};
  %ObjectSemTypes1  = %{$$DATA[12]};
  %SubjectSemTypes2 = %{$$DATA[11]};
  %ObjectSemTypes2  = %{$$DATA[13]};
  CreateSemtypePage($filename, "Semantic Type Distribution in Predication Differences",  
		    \%SubjectSemTypes1, \%ObjectSemTypes1, \%SubjectSemTypes2, \%ObjectSemTypes2);  
}

#########################################################################
sub CreateSemtypePage {
  my ($filename, $title, $ref1, $ref2, $ref3, $ref4) = @_;
  my (%SubjectSemTypes1) = %{$ref1};
  my (%ObjectSemTypes1)  = %{$ref2};
  my (%SubjectSemTypes2) = %{$ref3};
  my (%ObjectSemTypes2)  = %{$ref4};
  open(BARS, ">$filename") || die "can't open $filename.\n";
  print BARS "<HTML>\n";
  print BARS "<HEAD>\n";
  print BARS "<TITLE>$title</TITLE>\n";
  print BARS "</HEAD>\n";
  print BARS "<BODY>\n";

  foreach $key (sort keys %SemTypesH) {
      @widths = ($SubjectSemTypes1{$key}, $SubjectSemTypes2{$key},
                 $ObjectSemTypes1{$key}, $ObjectSemTypes2{$key});
      for (my $i=0; $i<4; $i++) {
          if (defined($widths[$i])) {
              # Keep track of min & max, for rescaling graphs
             if ($widths[$i] < $min) { $min = $widths[$i] }
             if ($widths[$i] > $max) { $max = $widths[$i] }
          }
      }
  }
  # This will consist of 2 nested tables.
  # The outer table has 2 columns, of which the left column
  #   contains the text labels for the semantic types,
  #   and the right column contains tables for the bar graphs
  #   (4 per semantic type).

  my @widths;
  my $scaledWidth;
  my $scale = 700;   # how wide I'll probably scale the widest to be
  my @colors = ('red', '#00FFFF', 'red', '#00FFFF');

  print BARS "<table height=\"20\">\n";

  # Print the key to the graphs
  # It is itself just an example set of 4 graphs
  print BARS "<a name=key>\n";
  print BARS '<tr>';
  print BARS "  <td>[semantic type]</td>\n  <td>";

  @widths = (400, 450, 380, 420);   # example values
  my @labels = ("subject in $semrep1",
                "subject in $semrep2",
                "object in $semrep1",
                "object in $semrep2");

  for (my $i=0; $i<4; $i++) {
	  $scaledWidth = $widths[$i];
  print BARS '<table height="10" width="' . $scaledWidth .
             '" cellSpacing="0" cellPadding="0" border="0">'."\n";
  print BARS '<tr><td bgColor="';
  # Color should be white if $scaledWidth is 0
  if ($scaledWidth < 1) { print BARS 'white' }
  else { print BARS $colors[$i] }
  print BARS "\">${labels[$i]}</td></tr></table>\n";
}

  # Print a very short table that is actually a separator
  print BARS "<tr><td>&nbsp;</td></tr>\n";
  print BARS "<tr><td colspan=2>\n";
  print BARS '  <table height="2" width="' . $scale .
             '" cellSpacing="0" cellPadding="0" border="0">'."\n";
  print BARS "    <tr><td bgColor=\"black\"></td></tr>\n  </table>\n";
  print BARS "</td></tr>\n";
  print BARS "<tr><td>&nbsp;</td></tr>\n\n";

  # Print the graphs
  # Each $key is a semantic type
  for $key (sort keys(%SemTypesH)) {
    # Note some of these values may be undefined
    @widths = ($SubjectSemTypes1{$key}, $SubjectSemTypes2{$key},
	       $ObjectSemTypes1{$key}, $ObjectSemTypes2{$key});

    my ($non_zero) = 0;
    for (my $i=0; $i<4; $i++) {
      if (defined($widths[$i]) && $widths[$i] > 0) {  $non_zero = 1; }
    } 
    if ($non_zero == 0) { next;}

    print BARS "<a name=$key>\n";
    # The outer table has 1 row per semantic type, containing 2 TDs
    print BARS "<tr>\n";
    # The left TD is the semantic type; the right TD has 4 tables
    print BARS "  <td>$key</td>\n  <td>";

    for (my $i=0; $i<4; $i++) {
      # XML standard requires "" around parameter values
      if (!defined($widths[$i])) { $widths[$i] = 0 }

      # Rescale
      $scaledWidth = $widths[$i];
      if ($max > $scale) {
	# Make it smaller
	if ($min > 0) {
	  $scaledWidth = 2*$scaledWidth / $min;
	} else {
	  $scaledWidth = 2*$scaledWidth;
	}
      }
      if ($max < $scale && $max > 0) {
	# Make it bigger
	$scaledWidth = $scale*$scaledWidth / $max;
      }

      print BARS '    <table height="10" width="' . $scaledWidth .
	'" cellSpacing="0" cellPadding="0" border="0">'."\n";
      print BARS '      <tr><td bgColor="';
      # Color should be white if $scaledWidth is 0
      if ($scaledWidth < 1) { print BARS 'white' }
      else { print BARS $colors[$i] }
      print BARS          "\">${widths[$i]}</td></tr></table>\n";
    }
    # The &nbsp; forces a vertical space of the outer table's height
    print BARS "  </td>\n</tr><tr><td>&nbsp;</td></tr></a>\n<p>\n";
  }

  print BARS "</table></BODY>\n";
  print BARS "</HTML>\n";
  close(BARS);
}

###############################################################
sub CreateEvaluationFile {
  my ($DIFFS) = @_;
  my (%CommonPredications) = %{$$DIFFS[2]};
  my (%LeftPredications)   = %{$$DIFFS[3]};
  my (%RightPredications)  = %{$$DIFFS[4]};
  my (@Citations)          = @{$$DIFFS[0]};
  my (%Utterances)         = %{$$DIFFS[1]};
  my (%CommonUtterances)   = %{$$DIFFS[14]};
  my ($filename) = $semrep1 . "_" . $semrep2 . "_evalres.html";
  open (EVAL, ">$filename") || die "can't open $filename";
  my ($TP) = 0;
  my ($FP) = 0;
  my ($FN) = 0;
  print EVAL "<HTML>\n";
  print EVAL "<HEAD>\n";
  print EVAL "<TITLE>Full Evaluation Results</TITLE>\n";
  print EVAL "</HEAD>\n";
  print EVAL "<BODY>\n";
  for $pmid (sort @Citations) {
    print EVAL "<br><br>----- Citation $pmid -----\n";
    foreach $utterance (sort UtteranceSort keys %{$CommonUtterances{$pmid}}) { 
      $sentence = $Utterances{$pmid}{$utterance};
      print EVAL "<br><br><a name=\"$utterance\"/>$sentence</a><br>\n";
      %CommonH = %{$CommonPredications{$pmid}{$utterance}};
      %LeftH = %{$LeftPredications{$pmid}{$utterance}};
      %RightH = %{$RightPredications{$pmid}{$utterance}};
      if (defined %CommonH) {
	foreach $predication (keys %CommonH) {
	  $predication_html = PredicationHTML($predication);
	  $cnt = $CommonH{$predication};
	  for ($i=0; $i < $cnt; $i++) {
	    print EVAL "<br><font color=\"orange\">TP</font>|" . $predication_html;
	    ++$TP;
	  }
	}
      }
      if (defined %LeftH) {
	foreach $predication (keys %LeftH) {
	  $predication_html = PredicationHTML($predication);
	  $cnt = $LeftH{$predication};
	  for ($i=0; $i < $cnt; $i++) {
	    print EVAL "<br><font color=\"magenta\">FP</font>|" . $predication_html;
	    ++$FP;
	  }
	}
      }  
      if (defined %RightH) {
	foreach $predication (keys %RightH) {
	  $predication_html = PredicationHTML($predication);
	  $cnt = $RightH{$predication};
	  for ($i=0; $i < $cnt; $i++) {
	    print EVAL "<br><font color=\"cyan\">FN</font>|" . $predication_html;
	    ++$FN;
	  }
	}
      }
    }
  }
  print EVAL "</BODY></HTML>\n";
  close(EVAL);  
  return ($TP, $FP, $FN);
}

############################################################
sub PredicationHTML {
  my ($predication ) = @_;
  @pred_elements = split(/\|/, $predication) ;
  $subj = $pred_elements[3];
  $subj_semtypes = $pred_elements[4];
  $subj_semtype = $pred_elements[5];
  $subj_cui = $pred_elements[2];
  $subj_geneid = $pred_elements[6];
  $subj_gene = $pred_elements[7];
  $predicate = $pred_elements[8];
  $obj = $pred_elements[10];
  $obj_semtypes = $pred_elements[11];
  $obj_semtype = $pred_elements[12];
  $obj_cui = $pred_elements[9];
  $obj_geneid = $pred_elements[13];
  $obj_gene = $pred_elements[14];
  $cnt = $Predications{$pmid}{$utterance}{$predication};
  return $utterance . "|relation|" . 
    $subj_cui . "|<font color=\"blue\">" . $subj . "</font>|" . $subj_semtypes . "|<font color=\"green\">" . $subj_semtype . "</font>|" . 
      $subj_geneid . "|<font color=\"blue\">" . $subj_gene . "</font>|<font color=\"red\">" . $predicate . "</font>|" . 
	$obj_cui  . "|<font color=\"blue\">" . $obj .  "</font>|" . $obj_semtypes  . "|<font color=\"green\">" . $obj_semtype  . "</font>|" . 
	  $obj_geneid  . "|<font color=\"blue\">" . $obj_gene . "</font><br>\n";
}


###############################################################
sub Union {
  my($ref1,$ref2) = @_;
  %hash1 = %{$ref1};
  %hash2 = %{$ref2};
  foreach $k (keys %hash2) {
    $hash1{$k}++;
  }
  @Union = keys %hash1;
  return \@Union;
}

###############################################################
sub IntersectHash {
  my($ref1,$ref2) = @_;
  %hash1 = %{$ref1};
  %hash2 = %{$ref2};
  %Intersection = ();
  foreach $k (keys %hash2) {
    if (exists $hash1{$k}) {
      $Intersection{$k} = $hash2{$k};
    }
  }
  return \%Intersection;
}

################################################################
sub FindDiffs {
  my ($DATA1, $DATA2) = @_;
  my (@AllCitations, %AllUtterances, %CommonUtterances);
  my (%CommonPredications, %LeftPredications, %RightPredications);
  my (%LeftSubjectSemtypes, %RightSubjectSemtypes, %LeftObjectSemtypes, %LeftSubjectSemtypes);
  my (%LeftPredicateCounts, %RightPredicateCounts, %LeftPredicateRelations, %RightPredicateRelations, %PredicateArguments);
#  my (@DIFFS) = (@AllCitations, %AllUtterances, %CommonPredications, %LeftPredications, %RightPredications,
#		      %LeftSubjectSemtypes, %RightSubjectSemtypes, %LeftObjectSemtypes, %RightObjectSemtypes);
  my (%Citations1)  =  %{$$DATA1[1]};
  my (%Citations2)  =  %{$$DATA2[1]};
  my (%CitationUtterances1) =  %{$$DATA1[3]};
  my (%CitationUtterances2) =  %{$$DATA2[3]}; 
  my (%UtterancePredications1)  =  %{$$DATA1[5]};
  my (%UtterancePredications2)  =  %{$$DATA2[5]};

  my (@AllCitations)  = @{Union(\%Citations1, \%Citations2)};
  foreach $pmid (@AllCitations) {
    %PMIDUtterances1 = %{$CitationUtterances1{$pmid}};
    %PMIDUtterances2 = %{$CitationUtterances2{$pmid}};
    if (%PMIDUtterances2) {
      %PMIDUtterances = %PMIDUtterances2;
      if (!PMIDUtterances1) {%CommonPMIDUtterances = %PMIDUtterances;}
      else {%CommonPMIDUtterances = %{IntersectHash(\%PMIDUtterances1,\%PMIDUtterances2)};}
    }
    else {
      if (%PMIDUtterances1) {%PMIDUtterances = %PMIDUtterances2;}
    }
    %{$AllUtterances{$pmid}} = %PMIDUtterances;
    %{$CommonUtterances{$pmid}} = %CommonPMIDUtterances;
    foreach $utterance (keys %PMIDUtterances) {
      %Predications1 = %{$UtterancePredications1{$pmid}{$utterance}};
      %Predications2 = %{$UtterancePredications2{$pmid}{$utterance}};
      if (!defined %Predications1 && defined %Predications2) { %{$RightPredications{$pmid}{$utterance}} = %Predications2; }
      elsif (defined %Predications1 && !defined %Predications2) { %{$LeftPredications{$pmid}{$utterance}} = %Predications1;}
      elsif (defined %Predications1 && defined %Predications2)  {
	$COMPARISON = ComparePredications(\%Predications1, \%Predications2);
	%common = %{$$COMPARISON[0]}; %left = %{$$COMPARISON[1]}; %right = %{$$COMPARISON[2]};
	%{$CommonPredications{$pmid}{$utterance}} = %common;
	%{$LeftPredications{$pmid}{$utterance}} = %left;
	%{$RightPredications{$pmid}{$utterance}} = %right;
      } 
    }
  }
  
  foreach $pmid (keys %LeftPredications) {
    %PMIDUtterances = %{$LeftPredications{$pmid}};
    foreach $utterance (keys %PMIDUtterances) {
      %Predications = %{$PMIDUtterances{$utterance}};
      foreach $pred (keys %Predications) {
	$count = $Predications{$pred};
	@els = split(/\|/,$pred);
	$predicate = $els[8];
	$subjSemType = $els[5];
	$objSemType = $els[12];
	$LeftSubjectSemtypes{$subjSemType} += $count;
	$LeftObjectSemtypes{$objSemType} += $count;
	$LeftPredicateCounts{$predicate} += $count;
	$SemTypePair    = $subjSemType . " " . $objSemType;
	$LeftPredicateRelations{$predicate}{$SemTypePair}{$utterance} += $count;
	$PredicateArguments{$predicate}{$SemTypePair} += $count;
      }
    }
  }
  foreach $pmid (keys %RightPredications) {
    %PMIDUtterances = %{$RightPredications{$pmid}};
    foreach $utterance (keys %PMIDUtterances) {
      %Predications = %{$RightPredications{$pmid}{$utterance}};
      foreach $pred (keys %Predications) {
	$count = $Predications{$pred};
	@els = split(/\|/,$pred);
	$predicate = $els[8];
	$subjSemType = $els[5];
	$objSemType = $els[12];
	$RightSubjectSemtypes{$subjSemType} += $count;
	$RightObjectSemtypes{$objSemType} += $count;
	$RightPredicateCounts{$predicate} += $count;
	$SemTypePair    = $subjSemType . " " . $objSemType;
	$RightPredicateRelations{$predicate}{$SemTypePair}{$utterance} += $count;
	$PredicateArguments{$predicate}{$SemTypePair} += $count;
      }
    }
  }

  my (@DIFFS) = ();
  push @DIFFS, [ @AllCitations ] ;
  push @DIFFS, { %AllUtterances };
  push @DIFFS, { %CommonPredications } ;
  push @DIFFS, { %LeftPredications } ;
  push @DIFFS, { %RightPredications } ;
  push @DIFFS, { %LeftPredicateCounts };
  push @DIFFS, { %RightPredicateCounts };
  push @DIFFS, { %LeftPredicateRelations };
  push @DIFFS, { %RightPredicateRelations };
  push @DIFFS, { %PredicateArguments };
  push @DIFFS, { %LeftSubjectSemtypes };
  push @DIFFS, { %RightSubjectSemtypes };
  push @DIFFS, { %LeftObjectSemtypes };
  push @DIFFS, { %RightObjectSemtypes };
  push @DIFFS, { %CommonUtterances };

  return \@DIFFS;
}

################################################################
sub CreateDiffPredicationPage {
  my ($DIFFS) = @_;
  my ($filename) = $semrep1 . "_" . $semrep2 . "_pred_diff.html";
  my ($filename_left) = $semrep1 . "_" . $semrep2 . "_pleft_diff.html";
  my ($filename_js) = $semrep1 . "_" . $semrep2 . "_diff.js";
  CreateMainFrameHtml($filename, $filename_left, "Comparison of Predication Differences in " . $semrep1 . " and " . $semrep2);
  CreateLeftFrameHtml($filename_left, $filename_js);
  CreateJSDiffFile($filename_js,$DIFFS);
}

################################################################
sub ReciprocalPredicate {
  my ($predication) = @_;
  @els = split(/\|/,$predication); 
  $predicate = $els[8];
  if ($predicate eq "COEXISTS_WITH") {return 1;}
  return 0;
}

################################################################

sub ReversePredication {
  my ($predication) = @_;
  @els = split(/\|/,$predication);
  return $els[0] . "|" . $els[1] . "|" . 
         $els[9] . "|" . $els[10] . "|" . $els[11]. "|" . $els[12] . "|" . $els[13] . "|" . $els[14] . "|" .
         $els[8] . "|" . 
         $els[2] . "|" . $els[3] . "|" . $els[4] . "|" . $els[5] . "|" . $els[6] .  "|" . $els[7];
}

################################################################
sub ComparePredications {
  my ($ref1, $ref2) = @_;
  my (%Predications1) = %{$ref1};
  my (%Predications2) = %{$ref2};
  my (%Common, %Left, %Right, %SEEN, $count1, $count2);
  foreach $k (keys %Predications1) {
    $count1 = $Predications1{$k};
    if ($Predications2{$k}) {$count2 = $Predications2{$k}; $SEEN{$k} = 1;} 
    elsif (ReciprocalPredicate($k)) {
       $k_inverse = ReversePredication($k);
       if ($Predications2{$k_inverse}) {
		$count2 = $Predications2{$k_inverse}; $SEEN{$k} = 1; $SEEN{$k_inverse} = 1;}
    }
    else {
      $count2 = 0;
      @els = split(/\|/,$k); $simple_predicate1 = $els[3] . "|" . $els[6] . "|" . $els[8] . "|" . $els[10] . "|" . $els[13];
      foreach $k2 (keys %Predications2) {
	@els2 = split(/\|/,$k2); $simple_predicate2 = $els2[3] . "|" . $els2[6] . "|" . $els2[8] . "|" . $els2[10] . "|" . $els2[13];
	if ($simple_predicate2 eq $simple_predicate1) { $count2 += $Predications2{$k2}; $SEEN{$k2} = 1;}
        elsif (ReciprocalPredicate($k)) {
	  $simple_predicate1_inverse = $els[10] . "|" . $els[13] . "|" . $els[8] . "|" . $els[3] . "|" . $els[6];
	  if ($simple_predicate2 eq $simple_predicate1_inverse) { $count2 += $Predications{$k2}; $SEEN{$k2} = 1; }
	}
      }
    }
    if ($count1 == $count2) {
      $Common{$k} = $count1;
    } 
    elsif ($count1 >= $count2 && $count2 > 0) {
      $Common{$k} = $count2;
      $Left{$k} = $count1 - $count2;
    }
    elsif ($count1 <= $count2) {
      $Common{$k} = $count1;
      $Right{$k} = $count2 - $count1;
    }
    elsif ($count2 == 0) {
      $Left{$k} = $count1;
    }
  }
  foreach $k (keys %Predications2) {
    unless ($SEEN{$k}) { 
      $Right{$k} = $Predications2{$k};
    }
  }
  my (@OUTPUT) = ();
  push @OUTPUT, { %Common };
  push @OUTPUT, { %Left   };
  push @OUTPUT, { %Right  };
 return \@OUTPUT;   
}

################################################################
sub UtteranceSort {
  $a =~ /(\d+).(ab|ti).(\d+)/;
  $a_pmid = $1;
  $a_abti = $2;
  $a_num = $3;
  $b =~ /(\d+).(ab|ti).(\d+)/;
  $b_pmid = $1;
  $b_abti = $2;
  $b_num = $3;
  $a_pmid <=> $b_pmid || 
   $b_abti cmp $a_abti ||
    $a_num <=> $b_num

}


