#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Downloads binary required to build Harbour on Windows
# Requires: curl, openssl, 7z

readonly mingwbase='https://downloads.sourceforge.net'; readonly option='-L'
curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.3.0/threads-posix/dwarf/i686-6.3.0-release-posix-dwarf-rt_v5-rev1.7z"
if openssl dgst -sha256 pack.bin | grep -q 8f7381e8ed61c438d36d33ae2f514a7ca8065c44dcf6801847fd425f71a9ee1d ; then
   7z e -y pack.bin mingw32/bin/mingw32-make.exe > /dev/null
   mv -f mingw32-make.exe win-make.exe
   chmod -x win-make.exe
fi
rm -f pack.bin
