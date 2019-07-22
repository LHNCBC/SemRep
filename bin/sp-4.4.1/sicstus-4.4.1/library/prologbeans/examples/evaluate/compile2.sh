#!/bin/sh
# The user can set the environment variable SPINSTALLDIR to override
# the SICStus installation used

if test -z $SPINSTALLDIR; then
    SPINSTALLDIR="../../../.."
fi
javac -classpath "$SPINSTALLDIR/bin/prologbeans.jar:." EvaluateGUI.java

# Build a runtime system. For usage see run2.sh.
$SPINSTALLDIR/../../bin/spld -o evaluate evaluate.pl
