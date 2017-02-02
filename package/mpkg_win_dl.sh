#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# - Requires '[PACKAGE]_VER' and '[PACKAGE]_HASH_[32|64]' envvars

case "$(uname)" in
   *_NT*)   readonly os='win';;
   linux*)  readonly os='linux';;
   Darwin*) readonly os='mac';;
   *BSD)    readonly os='bsd';;
esac

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${CI_BUILD_REF_NAME}${GIT_BRANCH}"

# Update/install MSYS2 pacman packages to fullfill dependencies

if [ "${os}" = 'win' ] ; then
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
fi

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

if [ "${os}" = 'win' ] ; then
(
   set -x

   if [ "${_BRANCH#*extmingw*}" != "${_BRANCH}" ] ; then
      readonly mingwbase='https://downloads.sourceforge.net'; readonly option='-L'
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.3.0/threads-posix/sjlj/i686-6.3.0-release-posix-sjlj-rt_v5-rev1.7z"
#     openssl dgst -sha256 pack.bin | grep -q ce5551a431661f3295a38fcc8563816a34e5cad867b3b35b1e802ef74e2c42f2
      curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.3.0/threads-posix/sjlj/x86_64-6.3.0-release-posix-sjlj-rt_v5-rev1.7z"
      openssl dgst -sha256 pack.bin | grep -q 10c40147b1781d0b915e96967becca99c6ffe2d56695a6830721051fe1b62b1f
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.3.0/threads-posix/dwarf/i686-6.3.0-release-posix-dwarf-rt_v5-rev1.7z"
#     openssl dgst -sha256 pack.bin | grep -q 8f7381e8ed61c438d36d33ae2f514a7ca8065c44dcf6801847fd425f71a9ee1d
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.3.0/threads-posix/seh/x86_64-6.3.0-release-posix-seh-rt_v5-rev1.7z"
#     openssl dgst -sha256 pack.bin | grep -q 2d0e72340ffa14916d4469db25c37889e477f8f1f49ba4f77155830ddc1dca89
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
fi

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
      if [ ! -d "${name}-mingw${plat}" ] ; then
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
      fi
   done
done

rm -f pack.bin pack.sig
