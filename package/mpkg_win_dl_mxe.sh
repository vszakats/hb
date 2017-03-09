#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Download and unpack a list of mxe binary packages
# along with the their dependencies.

# TOFIX: no checksum/signature verification whatsoever

mxe_get_pkg() {

  if [[ "$1" =~ ^(mxe-(i686|x86\-64)-(w64|unknown)-(mingw32|linux-gnu)(\.shared|\.static)?)-(.*)$ ]]; then
    repo="${BASH_REMATCH[1]}"  # mxe-x86-64-w64-mingw32.shared
    plat="${BASH_REMATCH[4]}"  # mingw32 | linux-gnu
    name="${BASH_REMATCH[6]}"  # harfbuzz
    if [ ! "${plat}" = 'linux-gnu' ] && \
       [ ! "${name}" = 'gcc' ]; then  # skip native packages and others that we don't need.

      inst="$(pwd)/usr/$(echo "${repo}" | sed -e 's|x86\-64|x86_64|' -e 's|^mxe-||')/installed/${name}"  # 'installed' file (not used)
      idid="${repo}-${name}"  # package id for internal purposes
      if [[ ! "${done}" = *"|${idid}|"* ]]; then  # avoid installing the same package twice
        done="${done} |${idid}|"  # add to list of install packages

        base='http://pkg.mxe.cc/repos/tar/'
        dirl="$(curl -fsS "${base}${repo}/")"  # HTML directory listing
        if [[ "${dirl}" =~ (${repo}-${name}_([0-9a-zA-Z.-]*).tar.xz) ]]; then
          vers="${BASH_REMATCH[2]}"  # 1.4.1
          echo "! Version: ${vers}"
          url="${base}${repo}/${BASH_REMATCH[1]}"  # <urlbase>/mxe-x86-64-w64-mingw32.shared-harfbuzz_1.4.1.tar.xz
          echo "! Downloading... '${url}'"
          if curl -fsS "${url}" | tar -x; then
            subd="$(echo "$(pwd)/usr/${repo}" | sed 's|^mxe-||' | sed 's|x86-64|x86_64|' | sed "s|${HOME}|~|")"
            echo "! OK. Unpacked into: '${subd}'"  # ~/mxe/usr/mxe-x86_64-w64-mingw32.shared
          else
            echo "! Error: Downlading/unpacking: '${url}'"
          fi

          ctrl="$(curl -fsS "http://pkg.mxe.cc/repos/deb-control/${repo}/${repo}-${name}_${vers}.deb-control" \
            | grep '^Depends: ' \
            | cut -c 10-)"

          for i in $(echo "${ctrl}" | sed "s/,/ /g"); do
            mxe_get_pkg "$i"  # recurse
          done
        else
          echo "! Error: Cannot find file in file list: '${repo}-${name}_*'"
        fi
      fi
    fi
  elif [ ! "$1" = 'mxe-requirements' ] && \
       [ ! "$1" = 'mxe-source' ]; then
    echo "! Error: Cannot parse package name: '$1'"
  fi
}

echo '! WARNING: No checksum/signature verification and cleartext download protocol.'

mkdir -p "${HOME}/mxe"
(
  cd "${HOME}/mxe" || exit

  done=''

  while [ -n "$1" ]; do
    echo "! Installing mxe package '$1'"
    mxe_get_pkg "$1"
    shift
  done

  echo "! Installed:$(echo "${done}" | sed 's/|//g')"
)
