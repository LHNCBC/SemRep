@echo off

rem Compile the sample code for MS Windows

SETLOCAL

rem The user can set the environment variable SPINSTALLDIR to override
rem the SICStus installation used

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

csc /reference:"%SPINSTALLDIR%\bin\prologbeans.dll" EvaluateGUI.cs

