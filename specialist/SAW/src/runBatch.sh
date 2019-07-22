#!/bin/bash

DIR=$1
#CPDIR=$2
OUTDIR=$2
for i in  `ls -1 $DIR/*.txt | xargs -n 1 basename`; do
#F=`echo "$i" | sed 's/\.ann/\.txt/g'`
F=`echo "$i" | sed 's/\.txt/\.semrep/g'`
FILE=$2/$F
echo $FILE
if [ -s "$FILE" ]; then
echo "$FILE is not empty" 
else 
	SAWenv ./a.out.Linux -DSFy --negex_st_set ALL $DIR/$i $FILE
	#SAWenv ./a.out.Linux -DSFy $DIR/$i $FILE
	#semrep -DSF $DIR/$i $FILE
fi
done;

