@echo off
if "%~1"=="info" goto info
if "%~1"=="params" goto params
if "%~1"=="paramloop" goto paramloop
if "%~1"=="defines" goto defines
if "%~1"=="prebuild" goto prebuild
if "%~1"=="postbuild" goto postbuild
if "%~1"=="prerelease" goto prerelease
if "%~1"=="postrelease" goto postrelease
if "%~1"=="help" goto help
goto :eof

:info
set project=ytd
set project_title=YTD
set project_url=https://ibv.github.io/YTD
set releasedir=..\
set exedir=..\Bin\
set srcdir=.\
set ext=.exe
set params=
set extralib=%srcdir%lib\RtmpDump;%srcdir%lib\msdl\src
set cli=1
set gui=1
set setup=1
set xxx=1
exit /b 0
goto :eof

:help
echo    cli/nocli ....... Include/exclude CLI support.
echo    gui/nogui ....... Include/exclude GUI support.
echo    xxx/noxxx ....... Include/exclude XXX providers
goto :eof

:params
goto :eof

:paramloop
if /i "%~2"=="cli" set cli=1
if /i "%~2"=="nocli" set cli=0
if /i "%~2"=="gui" set gui=1
if /i "%~2"=="nogui" set gui=0
if /i "%~2"=="setup" set setup=1
if /i "%~2"=="nosetup" set setup=0
if /i "%~2"=="xxx" set xxx=1
if /i "%~2"=="noxxx" set xxx=0
goto :eof

:defines
if not "%cli%"=="1" set defs=%defs% -dNO_CLI
if not "%gui%"=="1" set defs=%defs% -dNO_GUI
if not "%setup%"=="1" set defs=%defs% -dNO_SETUP
if not "%xxx%"=="1" set defs=%defs% -dNO_XXX
goto :eof

:prebuild
goto :eof

:postbuild
if exist "%exedir%locale\." rd /s /q "%exedir%locale"
md "%exedir%locale"
xcopy "%srcdir%locale\*.mo" "%exedir%locale" /s /i >nul
goto :eof

:prerelease
if exist "%exedir%%project%.xml" del /q "%exedir%%project%.xml" >nul
goto :eof

:postrelease
goto :eof
