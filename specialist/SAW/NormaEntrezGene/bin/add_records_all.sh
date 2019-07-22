#!/bin/sh
DIR=/net/pax/export/home/halil/NormaEntrezGene
LIB=$DIR/lib
LD_LIBRARY_PATH=/home/halil/db-3.0.55/build_unix/.libs:$LD_LIBRARY_PATH:$LIB
DB_DIR=/net/cgsb-filer2/vol/vol2/aux/ind/pax
export LD_LIBRARY_PATH 
echo $LD_LIBRARY_PATH

#mkdir $DB_DIR/entrezgene2
java -classpath $LIB/db-3.0.55.jar:$LIB/entrezgene.jar entrezgene.AddEntrezGeneRecords -e $DB_DIR/entrezgene_all -s $1 -a $2 -t ALL_SYMBOL -b ALL_ALIAS  
#mv $DB_DIR/entrezgene $DB_DIR/entrezgene_old
#mv $DB_DIR/entrezgene2 $DB_DIR/entrezgene

