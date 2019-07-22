@echo off

rem Run the Java side of the sample


SETLOCAL

rem The user can set the environment variable SPINSTALLDIR to override
rem the SICStus installation used

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

echo Starting Java client
java -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;." PBTest

