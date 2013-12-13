@echo off

rem configurations 
set PEGASUS_LIB=%CD%\lib

set SAGITTARIUS_DIR=%programfiles%\Sagittarius
rem by default
set SASH=%SAGITTARIUS_DIR%\sash.exe
set INSTALL=%SAGITTARIUS_DIR%

rem TODO accept arguments
rem TODO get target from command line

rem dump config
echo Sagittarius directory %SAGITTARIUS_DIR%
echo Installing  library   %PEGASUS_LIB%

"%SASH%" -L%PEGASUS_LIB% .\bin\install.scm -l lib -w . -t install

rem script
set PEGASUS_BIN=%INSTALL%\pegasus.bat
set PEGASUS_SCM=%INSTALL%\pegasus.scm
echo -- Creating pegasus file

echo @echo off > "%PEGASUS_BIN%"
echo "%SASH%" "%PEGASUS_SCM%" "%%*" >> "%PEGASUS_BIN%"

echo -- Installing: %PEGASUS_BIN%

copy .\pegasus.bat "%PEGASUS_BIN%"
copy .\bin\pegasus.scm "%PEGASUS_SCM%"

del pegasus.bat

"%PEGASUS_BIN%" init

echo Done!