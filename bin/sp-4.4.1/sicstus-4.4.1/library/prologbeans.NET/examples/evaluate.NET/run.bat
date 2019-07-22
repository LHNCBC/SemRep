@echo off

rem Run the C# side of the sample


SETLOCAL

rem The user can set the environment variables SPINSTALLDIR and/or SPBINDIR
rem to override the SICStus installation used

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..
if "%SPBINDIR%"=="" set SPBINDIR="%SPINSTALLDIR%\bin"

copy "%SPBINDIR%\prologbeans.dll" .

echo Starting .NET C# client
echo You need to start Prolog server separately using something like:
echo sicstus -l ../../../prologbeans/examples/evaluate/evaluate --goal "main."

.\EvaluateGUI


