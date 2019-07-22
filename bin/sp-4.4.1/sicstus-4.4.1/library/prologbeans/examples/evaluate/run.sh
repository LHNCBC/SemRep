#!/bin/sh
# The user can set the environment variable SPJARDIR to override the
# location of prologbeans.jar

: ${SPJARDIR="../../../../bin"}

echo Starting Java client
echo You need to start Prolog server separately using something like:
echo sicstus -l evaluate --goal "main."

java -classpath "$SPJARDIR/prologbeans.jar:." EvaluateGUI
