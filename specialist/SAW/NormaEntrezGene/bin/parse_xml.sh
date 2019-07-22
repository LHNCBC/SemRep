#!/bin/sh
DIR=/net/pax/export/home/halil/NormaEntrezGene
LIB=$DIR/lib
LD_LIBRARY_PATH=/home/halil/db-3.0.55/build_unix/.libs:$LD_LIBRARY_PATH:$LIB
export LD_LIBRARY_PATH 
echo $LD_LIBRARY_PATH

java -classpath $LIB/db-3.0.55.jar:$LIB/thirdparty.jar:$LIB/entrezgene.jar entrezgene.ParseEntrezGene -f $1 -s $2 -a $3

