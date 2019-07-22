#!/bin/sh
# The user can set the environment variable SPINSTALLDIR to override
# the SICStus installation used

if test -z $SPINSTALLDIR; then
    SPINSTALLDIR="../../../.."
fi
javac -classpath "$SPINSTALLDIR/bin/prologbeans.jar:." PBTest.java
