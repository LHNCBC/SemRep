#!/bin/sh
# The user can set the environment variable SPJARDIR to override the
# location of prologbeans.jar

: ${SPJARDIR="../../../../bin"}

echo Starting Java client
echo You need to start the Prolog runtime server separately using something
echo like:
echo evaluate

java -classpath "$SPJARDIR/prologbeans.jar:." EvaluateGUI
