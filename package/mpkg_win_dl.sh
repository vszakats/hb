#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# - Requires '[PACKAGE]_VER' and '[PACKAGE]_HASH_[32|64]' envvars
# - Requires bash extensions for curly brace expansion, but using
#   'sh' anyway to stay in POSIX shell mode with shellcheck.

case "$(uname)" in
  *_NT*)   readonly os='win';;
  Linux*)  readonly os='linux';;
  Darwin*) readonly os='mac';;
  *BSD)    readonly os='bsd';;
esac

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${CI_BUILD_REF_NAME}${GIT_BRANCH}"
[ -n "${_BRANCH}" ] || _BRANCH="$(git symbolic-ref --short --quiet HEAD)"
[ -n "${_BRANCH}" ] || _BRANCH='master'
[ -n "${CC}" ] || CC="${_BRANCH}"
CC4="$(echo "${CC}" | cut -c -4)"

# Update/install packages to fulfill dependencies

if [ "${os}" = 'win' ]; then
  if [ "${CC4}" = 'msvc' ]; then
    # Experimental, untested, requires 2015 Update 3 or upper
    # bzip2 cairo expat freeimage icu libmariadb libpng libpq libssh2 lzo pcre pcre2 qt5 sqlite3 zlib
    echo '.'
    #vcpkg install --no-sendmetrics \
    #  curl curl:x64-windows openssl openssl:x64-windows
    #vcpkg integrate install
  else
    # MSYS2 pacman

    # clang toolchain
    if [ "${CC#*clang*}" != "${CC}" ]; then
      pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-clang
    fi

    # Dependencies of the default (full) list of contribs
    if [ "${_BRANCH#*prod*}" = "${_BRANCH}" ]; then
      pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{cairo,freeimage,gd,ghostscript,libmariadbclient,libyaml,postgresql,rabbitmq-c}
    # pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-qt5
    fi

    # Skip using this component for test purposes for now in favour of creating
    # more practical/usable snapshot binaries.
    # pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-icu

    # Dependencies of 'prod' builds (our own builds are used instead for now)
    # pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{curl,openssl}

    # Dependencies of 'prod' builds (vendored sources are used instead for now)
    # pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{bzip2,expat,libharu,lzo2,sqlite3}

    # Core dependencies (vendored sources are used instead for now)
    # pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{libpng,pcre2,pcre,zlib}
  fi
fi

# Install packages manually

set | grep '_VER='

# Quit if any of the lines fail
set -e

alias curl='curl -fsS --connect-timeout 15 --retry 3'
alias gpg='gpg --batch --keyid-format LONG'

gpg_recv_keys() {
  req="pks/lookup?search=0x$1&op=get"
  (
    set -x
    if ! curl "https://pgp.mit.edu/${req}" | gpg --import --status-fd 1; then
      curl "https://sks-keyservers.net/${req}" | gpg --import --status-fd 1
    fi
  )
}

gpg --version | grep gpg

# Dependencies for the Windows distro package

if [ "${os}" = 'win' ]; then
(
  set -x

  if [ "${_BRANCH#*mingwext*}" != "${_BRANCH}" ]; then
    curl -o pack.bin -L --proto-redir =https "https://downloads.sourceforge.net/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/7.1.0/threads-posix/sjlj/x86_64-7.1.0-release-posix-sjlj-rt_v5-rev0.7z"
    openssl dgst -sha256 pack.bin | grep -q a117ec6126c9cc31e89498441d66af3daef59439c36686e80cebf29786e17c13
    7z x -y pack.bin > /dev/null
  fi
)
fi

# Dependencies for Windows builds

if [ "${CC4}" != 'msvc' ]; then

  # Bintray public key
  gpg_recv_keys 8756C4F765C9AC3CB6B85D62379CE192D401AB61

  # Builder public key
  # curl 'https://bintray.com/user/downloadSubjectPublicKey?username=vszakats' \
  #   | gpg --import

  readonly base='https://bintray.com/artifact/download/vszakats/generic/'

  for plat in '32' '64'; do
    for name in \
      'nghttp2' \
      'openssl' \
      'libssh2' \
      'curl' \
    ; do
      suffix=''
      if [ ! -d "${name}-mingw${plat}" ]; then
        eval ver="\$$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_VER"
        eval hash="\$$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_HASH_${plat}"
        # shellcheck disable=SC2154
        (
          set -x
          curl -L --proto-redir =https \
            -o pack.bin "${base}${name}-${ver}-win${plat}-mingw${suffix}.7z" \
            -o pack.sig "${base}${name}-${ver}-win${plat}-mingw${suffix}.7z.asc"
          gpg --verify-options show-primary-uid-only --verify pack.sig pack.bin
          openssl dgst -sha256 pack.bin | grep -q "${hash}" || exit 1
          7z x -y pack.bin > /dev/null
          mv "${name}-${ver}-win${plat}-mingw" "${name}-mingw${plat}"
        )
      fi
    done
  done

  rm -f pack.bin pack.sig
fi
