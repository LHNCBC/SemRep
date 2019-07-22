#!/bin/sh
# The user can set the environment variable SPJARDIR to override the
# location of prologbeans.jar

: ${SPJARDIR="../../../../bin"}

echo Starting Java client
java -classpath "$SPJARDIR/prologbeans.jar:." PBTest
