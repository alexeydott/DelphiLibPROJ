@ECHO OFF

SET BCCROOT=d:\rad113
SET ROOTDIR=%~dp0

if /i "DEBUG"=="%~1" set "build_type=DEBUG"

SET IMPLIB=%BCCROOT%lib\win32\release\cw32mt.lib
SET IMPLIB64=%BCCROOT%lib\win64\release\cw64mt.a

rem SET PATH=D:\develop\tools\cmake\bin;%PATH%
call %BCCROOT%bin\rsvars.bat