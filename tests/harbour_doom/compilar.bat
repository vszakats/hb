@echo off
setlocal EnableExtensions EnableDelayedExpansion
cd /d "%~dp0"

set "HB_BIN=c:\projetos\git\hb\bin\win\mingw64"
if not exist "%HB_BIN%\hbmk2.exe" set "HB_BIN=C:\harbour_msys\bin"
if not exist "%HB_BIN%\hbmk2.exe" (
    echo ERRO: hbmk2 64 bits nao encontrado.
    echo Compile o Harbour com compilar.bat na raiz do repositorio.
    exit /b 1
)
if not exist "C:\msys64\ucrt64\bin\gcc.exe" (
    echo ERRO: GCC do UCRT64 nao encontrado em C:\msys64\ucrt64\bin
    exit /b 1
)
if not exist "sdl3\include\SDL3\SDL.h" (
    echo ERRO: cabecalhos SDL3 nao encontrados em sdl3\include
    exit /b 1
)
if not exist "sdl3\SDL3-3.4.18-win32-x64\SDL3.dll" (
    echo ERRO: SDL3.dll 64 bits nao encontrada em sdl3\SDL3-3.4.18-win32-x64
    exit /b 1
)
if not exist "sdl3\lib\SDL3.def" (
    echo ERRO: SDL3.def nao encontrado em sdl3\lib
    exit /b 1
)

set "PATH=%HB_BIN%;C:\msys64\ucrt64\bin;C:\Program Files\Git\cmd;%PATH%"
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
set "HB_PLATFORM=win"

if not exist "sdl3\lib64" mkdir "sdl3\lib64"
if not exist "sdl3\lib64\libSDL3.dll.a" (
    echo Gerando libSDL3.dll.a de 64 bits...
    dlltool -m i386:x86-64 -D SDL3.dll -d "sdl3\lib\SDL3.def" -l "sdl3\lib64\libSDL3.dll.a"
    if errorlevel 1 (
        echo ERRO: dlltool nao conseguiu gerar a biblioteca de importacao.
        exit /b 1
    )
)

echo Harbour: %HB_BIN%
echo gcc:
gcc -dumpversion
gcc -dumpmachine
echo.

hbmk2 -comp=mingw64 doom_hb_ucrt64.hbp %*
if errorlevel 1 (
    echo Compilacao falhou.
    exit /b 1
)

copy /Y "sdl3\SDL3-3.4.18-win32-x64\SDL3.dll" "SDL3.dll" >nul

echo.
echo Pronto: Doom_hb.exe
echo WADs nesta pasta: DOOM1.WAD, Doom2.wad, freedoom1.wad
endlocal
