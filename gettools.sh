#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2016-2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Downloads binaries required to build Harbour on non-*nix platforms
# Requires: curl, 7z, unzip
# Usage: ./gettools.sh [win|os2|dos]

cd "$(dirname "$0")" || exit

os="$1"
if [ -z "${os}" ]; then
  case "$(uname)" in
    *_NT*)    readonly os='win';;
    *OS/2*)   readonly os='os2';;
    *MS-DOS*) readonly os='dos';;
  esac
fi

if [ -n "${os}" ]; then

  rm -rf "./_${os}"
  mkdir "_${os}" || exit
  (
    cd "_${os}" || exit

    case "${os}" in
      win)
        curl -f -o pack.bin 'https://sourceforge.mirrorservice.org/m/mi/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.3.0/threads-posix/dwarf/i686-6.3.0-release-posix-dwarf-rt_v5-rev1.7z'
        7z e -y pack.bin mingw32/bin/mingw32-make.exe > /dev/null
        mv -f mingw32-make.exe ../win-make.exe
        chmod -x ../win-make.exe
        rm -f pack.bin
        ;;
      os2)
        curl -f -O https://dl.dropboxusercontent.com/u/76425158/coreutils-8.8-os2-20101223.zip
        curl -f -O http://hobbes.nmsu.edu/download/pub/os2/dev/util/make-3.81-r3-bin-static.zip
        mkprefix='usr/'
        tlprefix='coreutils/'
        ;;
      dos)
        curl -f -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2gnu/mak421b.zip
        curl -f -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2gnu/fil41br2.zip
        curl -f -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2gnu/shl2011br2.zip
        curl -f -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2/djdev205.zip
        unset mkprefix
        unset tlprefix
        ;;
    esac

    if ls ./*.zip > /dev/null 2>&1; then
      for file in ./*.zip; do
        unzip -q -o "${file}"
        rm -f "${file}"
      done

      cp -p "${mkprefix}bin/make.exe"  "../${os}-make.exe"
      cp -p "${tlprefix}bin/mkdir.exe" "../config/${os}mkdir.exe"
      cp -p "${tlprefix}bin/rm.exe"    "../config/${os}rm.exe"
      cp -p "${tlprefix}bin/cp.exe"    "../config/${os}cp.exe"
      cp -p "${tlprefix}bin/echo.exe"  "../config/${os}echo.exe"
    fi
  )

  rm -rf "./_${os}"
fi
