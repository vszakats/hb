@echo off
setlocal
cd /d "%~dp0"

if exist "c:\mingw-4.8.1\start.bat" (
    call "c:\mingw-4.8.1\start.bat"
    cd /d "%~dp0"
)

SET PATH=C:\HARBOUR\BIN;C:\MINGW-4.8.1\BIN;%PATH%
SET INCLUDE=c:\mingw-4.8.1\lib\gcc\mingw32\4.8.1\include
SET CPATH=C:\MINGW-4.8.1
SET C_INCLUDE_PATH=%CD%;C:\MINGW-4.8.1\INCLUDE;C:\HARBOUR\INCLUDE
SET CPLUS_INCLUDE_PATH=%CD%;C:\MINGW-4.8.1\INCLUDE;C:\HARBOUR\INCLUDE
SET COBJC_INCLUDE_PATH=%CD%;C:\MINGW-4.8.1\INCLUDE;C:\HARBOUR\INCLUDE
SET C_LIB_PATH=C:\MINGW-4.8.1\LIB;C:\HARBOUR\LIB

where hbmk2 >nul 2>&1
if errorlevel 1 (
    echo ERRO: hbmk2 nao encontrado. Verifique C:\HARBOUR\BIN no PATH.
    exit /b 1
)

echo Compilando doom_hb com hbmk2 -lxhb...
hbmk2 -lxhb doom_hb.hbp %*
if errorlevel 1 (
    echo Compilacao falhou.
    exit /b 1
)

copy /Y "sdl3\SDL3-3.4.18-win32-x86\SDL3.dll" "SDL3.dll" >nul
echo.
echo Pronto: Doom_hb.exe
echo Coloque DOOM1.WAD nesta pasta.
endlocal
