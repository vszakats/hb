@echo off
setlocal EnableExtensions EnableDelayedExpansion
cd /d "%~dp0"

if not exist "C:\msys64\ucrt64\bin\gcc.exe" (
    echo GCC do UCRT64 nao encontrado em C:\msys64\ucrt64\bin
    exit /b 1
)
if not exist "C:\msys64\ucrt64\bin\mingw32-make.exe" (
    echo mingw32-make nao encontrado em C:\msys64\ucrt64\bin
    exit /b 1
)

rem UCRT64 primeiro. Git for Windows antes do git do MSYS, que trava no git diff do makefile.
set "PATH=C:\msys64\ucrt64\bin;C:\Program Files\Git\cmd;C:\msys64\usr\bin;%PATH%"

set "NEWPATH="
for %%A in ("%PATH:;=";"%") do (
    set "ITEM=%%~A"
    echo !ITEM! | findstr /I /C:"mingw-4.8.1" >nul
    if errorlevel 1 (
        if defined NEWPATH (
            set "NEWPATH=!NEWPATH!;!ITEM!"
        ) else (
            set "NEWPATH=!ITEM!"
        )
    )
)
set "PATH=!NEWPATH!"

set "HB_COMPILER=mingw64"
set "HB_USER_CFLAGS=-march=x86-64-v2 -mtune=generic"
set "HB_INSTALL_PREFIX=C:\harbour_msys"

echo gcc:
gcc -dumpversion
gcc -dumpmachine
echo Instalacao: %HB_INSTALL_PREFIX%
echo.

if "%~1"=="" (
    mingw32-make -j8
    if errorlevel 1 exit /b 1
    mingw32-make install
) else (
    mingw32-make -j8 %*
)
exit /b %ERRORLEVEL%
