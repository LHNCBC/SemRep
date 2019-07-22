#!/bin/bash 
# ARGS
# ARG1: The directory where the text files are located
# ARG2: The output directory for GENSPEC btree files
# ARG3: The number of text files.
PERL=~/perl-5.8.9/perl
#PERL=perl
EXEC=create_hrel_btree.pl
IN_DIR=$1
OUT_DIR=$2
FILE_COUNT=$3

i=1

while [ $i -le ${FILE_COUNT} ]
do
    $PERL $EXEC -bt $OUT_DIR/hrel-UMLS_2006AA.btree_$i $IN_DIR/hrel-UMLS_2006AA_$i
    echo "Completed loop $i"
    i=`expr $i + 1`
done
