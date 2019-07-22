@echo off

rem Run the Java side of the sample


SETLOCAL

rem The user can set the environment variable SPINSTALLDIR to override
rem the SICStus installation used

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

echo Starting Java client
echo You need to start Prolog server separately using something like:
echo sicstus -l evaluate --goal "main."

java -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;." EvaluateGUI

