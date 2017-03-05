#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Download and unpack an mxe binary package

# TOFIX: no checksum/signature verification whatsoever

mxe_get_pkg() {
  if [[ "$1" =~ ^(mxe-(i686|x86\-64)-w64-mingw32\.(shared|static))-(.*)$ ]]; then
    repo="${BASH_REMATCH[1]}"  # mxe-x86-64-w64-mingw32.shared
    name="${BASH_REMATCH[4]}"  # harfbuzz
    base='http://pkg.mxe.cc/repos/tar/'
    dirl="$(curl -fsS "${base}${repo}/")"  # HTML directory listing
    if [[ "${dirl}" =~ (${repo}-${name}_([0-9a-zA-Z.-]*).tar.xz) ]]; then
      echo "! Version: ${BASH_REMATCH[2]}"     # 1.4.1
      url="${base}${repo}/${BASH_REMATCH[1]}"  # <urlbase>/mxe-x86-64-w64-mingw32.shared-harfbuzz_1.4.1.tar.xz
      echo "! Downloading... '${url}'"
      if curl -fsS "${url}" | tar -x; then
        subd="$(echo "$(pwd)/usr/${repo}" | sed 's|^mxe-||' | sed 's|x86-64|x86_64|' | sed "s|${HOME}|~|")"
        echo "! OK. Unpacked into: '${subd}'"  # ~/mxe/usr/mxe-x86_64-w64-mingw32.shared
      fi
    fi
  fi
}

echo '! WARNING: No checksum/signature verification and cleartext download protocol.'

mkdir -p "${HOME}/mxe"
(
  cd "${HOME}/mxe" || exit

  while [ -n "$1" ]; do
    echo "! Installing mxe package '$1'"
    mxe_get_pkg "$1"
    shift
  done
)
