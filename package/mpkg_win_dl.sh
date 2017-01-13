#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# - Requires '[PACKAGE]_VER' and '[PACKAGE]_HASH_[32|64]' envvars

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${CI_BUILD_REF_NAME}${GIT_BRANCH}"

# Update/install MSYS2 pacman packages to fullfill dependencies

# Dependencies of the default (full) list of contribs
if [ "${_BRANCH#*prod*}" = "${_BRANCH}" ] ; then
   pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{cairo,freeimage,gd,ghostscript,libmariadbclient,postgresql}
#  pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-qt5
fi

# Skip using this component for test purposes for now in favour
# of creating more practical/usable snapshot binaries.
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-icu

# Dependencies of 'prod' builds (we use our own builds instead for now)
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{curl,openssl}

# Dependencies of 'prod' builds (we use vendored sources instead for now)
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{bzip2,expat,libharu,lzo2,sqlite3}

# Dependencies of Harbour core (we use vendored sources instead for now)
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{libpng,pcre,zlib}

# Install packages manually

set | grep '_VER='

# Quit if any of the lines fail
set -e

alias curl='curl -fsS --connect-timeout 15 --retry 3'
alias gpg='gpg --batch --keyid-format LONG'

gpg_recv_keys() {
   if ! gpg --keyserver hkps://pgp.mit.edu --recv-keys "$@" ; then
      gpg --keyserver hkps://sks-keyservers.net --recv-keys "$@"
   fi
}

gpg --version | grep gpg

# Dependencies for the Windows distro package

(
   set -x

#  curl -o pack.bin -L --proto-redir =https 'https://github.com/upx/upx/releases/download/v3.92/upx392w.zip'
#  openssl dgst -sha256 pack.bin | grep -q 041f9fe5c749a5491db1c902db16b55a6d343030103cb1add2fcc5bb63c6143d
#  7z e -y -oupx pack.bin > /dev/null

   if [ "${_BRANCH#*extmingw*}" != "${_BRANCH}" ] ; then
      readonly mingwbase='https://downloads.sourceforge.net'; readonly option='-L'
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.3.0/threads-posix/sjlj/i686-6.3.0-release-posix-sjlj-rt_v5-rev0.7z"
#     openssl dgst -sha256 pack.bin | grep -q 73af8bd6aa0863e4b8c060e0f2d21fdff6d3d9e39adfa13fd412ddca8c8c6c21
      curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.3.0/threads-posix/sjlj/x86_64-6.3.0-release-posix-sjlj-rt_v5-rev0.7z"
      openssl dgst -sha256 pack.bin | grep -q 41cab7c322058bec6ff02f461cd4327ed68839719c06bfc74112614c4999aae8
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.3.0/threads-posix/dwarf/i686-6.3.0-release-posix-dwarf-rt_v5-rev0.7z"
#     openssl dgst -sha256 pack.bin | grep -q 5ca7dba4cb9719c75faf4c29f78c70ff9bd8b663737ef36f9283c47c27cb6246
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.3.0/threads-posix/seh/x86_64-6.3.0-release-posix-seh-rt_v5-rev0.7z"
#     openssl dgst -sha256 pack.bin | grep -q c6151102368c3fa3a0f316b7cf7775d898dee641aa1ccc3516ff7adeecb2b9e4
      # Will unpack into "./mingw64"
      7z x -y pack.bin > /dev/null
   else
      # Bad hack to avoid duplicate manifests being linked into slightly "off" binaries.
      #    https://github.com/Alexpux/MSYS2-packages/issues/454
      #    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=69880
      for file in \
         /usr/lib/default-manifest.o \
         /mingw32/i686-w64-mingw32/lib/default-manifest.o \
         /mingw64/x86_64-w64-mingw32/lib/default-manifest.o ; do
         [ -f "${file}" ] && mv -f "${file}" "${file}-ORI"
      done
   fi
)

# Dependencies for Windows builds

# Bintray public key
gpg_recv_keys 8756C4F765C9AC3CB6B85D62379CE192D401AB61

# Builder public key
# curl 'https://bintray.com/user/downloadSubjectPublicKey?username=vszakats' \
#    | gpg --import

readonly base='https://bintray.com/artifact/download/vszakats/generic/'

for plat in '32' '64' ; do
   for name in \
      'nghttp2' \
      'openssl' \
      'libssh2' \
      'curl' \
   ; do
      eval ver="\$$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_VER"
      eval hash="\$$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_HASH_${plat}"
      # shellcheck disable=SC2154
      (
         set -x
         curl -o pack.bin -L --proto-redir =https "${base}${name}-${ver}-win${plat}-mingw.7z"
         curl -o pack.sig -L --proto-redir =https "${base}${name}-${ver}-win${plat}-mingw.7z.asc"
         gpg --verify-options show-primary-uid-only --verify pack.sig pack.bin
         openssl dgst -sha256 pack.bin | grep -q "${hash}"
         7z x -y pack.bin > /dev/null
         mv "${name}-${ver}-win${plat}-mingw" "${name}-mingw${plat}"
      )
   done
done
