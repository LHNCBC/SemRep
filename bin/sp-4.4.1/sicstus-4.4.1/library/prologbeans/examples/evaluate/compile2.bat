@echo off

rem Compile the sample code for MS Windows and build a runtime system

SETLOCAL

rem The user can set the environment variable SPINSTALLDIR to override
rem the SICStus installation used

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

javac -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;." EvaluateGUI.java

# Build a runtime system. For usage see run2.bat.
%SPINSTALLDIR%\bin\spld evaluate.pl
