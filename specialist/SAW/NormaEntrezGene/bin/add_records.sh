#!/bin/sh
DIR=..
LIB=$DIR/lib
BERKELEY_JAVA_DIR=~/db-4.8.24.NC/build_unix
LD_LIBRARY_PATH=$BERKELEY_JAVA_DIR/.libs:$LD_LIBRARY_PATH:$LIB
#DB_DIR=/net/cgsb-filer2/vol/vol2/aux/ind/pax
DB_DIR=$DIR/DB
export LD_LIBRARY_PATH 
echo $LD_LIBRARY_PATH

#mkdir $DB_DIR
java -classpath $BERKELEY_JAVA_DIR/db.jar:$LIB/entrezgene.jar entrezgene.AddEntrezGeneRecords -e $DB_DIR -s $1 -a $2 -t HUMAN_SYMBOL -b HUMAN_ALIAS  
#mv $DB_DIR/entrezgene $DB_DIR/entrezgene_old
#mv $DB_DIR/entrezgene2 $DB_DIR/entrezgene

