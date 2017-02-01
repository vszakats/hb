#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2016-2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Downloads binaries required to build Harbour on MS-DOS and OS/2
# Requires: curl

cd "$(dirname "$0")" || exit

for os in dos os2; do

  mkdir "_${os}" || exit
  (
    cd "_${os}" || exit

    case "${os}" in
      dos)
        curl -fsS -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2gnu/mak421b.zip
        curl -fsS -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2gnu/fil41br2.zip
        curl -fsS -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2gnu/shl2011br2.zip
        curl -fsS -O https://mirrorservice.org/sites/ftp.delorie.com/pub/djgpp/current/v2/djdev205.zip
        mkprefix=''
        tlprefix=''
        ;;
      os2)
        curl -fsS -O https://dl.dropboxusercontent.com/u/76425158/coreutils-8.8-os2-20101223.zip
        curl -fsS -O http://hobbes.nmsu.edu/download/pub/os2/dev/util/make-3.81-r3-bin-static.zip
        mkprefix='usr/'
        tlprefix='coreutils/'
        ;;
    esac

    for file in ./*.zip; do
      unzip -o "${file}"
      rm -f "${file}"
    done

    cp -p "${mkprefix}bin/make.exe"  "../../${os}-make.exe"
    cp -p "${tlprefix}bin/mkdir.exe" "../${os}mkdir.exe"
    cp -p "${tlprefix}bin/rm.exe"    "../${os}rm.exe"
    cp -p "${tlprefix}bin/cp.exe"    "../${os}cp.exe"
    cp -p "${tlprefix}bin/echo.exe"  "../${os}echo.exe"
  )

  rm -rf "_${os}"
done
