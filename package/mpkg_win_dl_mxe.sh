#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net)
# License: The MIT license (MIT)
# ---------------------------------------------------------------

# Download and unpack a list of mxe binary packages along with the their
# dependencies. Requires `ar` tool, which comes with macOS.

mxe_get_pkg() {

  if [[ "$1" =~ ^(mxe-(i686|x86\-64)-(w64|unknown)-(mingw32|linux-gnu)(\.shared|\.static)?)-(.*)$ ]]; then

    repo="${BASH_REMATCH[1]}"  # mxe-x86-64-w64-mingw32.shared
    plat="${BASH_REMATCH[4]}"  # mingw32 | linux-gnu
    name="${BASH_REMATCH[6]}"  # harfbuzz

    if [ ! "${plat}" = 'linux-gnu' ] && \
       [ ! "${name}" = 'gcc' ]; then  # skip native packages and others that we don't need.

      idid="${repo}-${name}"  # package id for internal purposes
      if [[ ! "${done}" = *"|${idid}|"* ]]; then  # avoid installing the same package twice
        done="${done} |${idid}|"  # add to list of install packages

        sect="$(awk "/^Package: ${repo}-${name}$/,/^SHA256: /" Packages)"  # control section for this package

        debu="$(echo "${sect}" | sed -n -E 's,^Filename: (.+)$,\1,p')"  # .deb path
        vers="$(echo "${sect}" | sed -n -E 's,^Version: (.+)$,\1,p')"  # package version
        hash="$(echo "${sect}" | sed -n -E 's,^SHA256: ([0-9a-fA-F]{64})$,\1,p')"  # .deb hash
        dept="$(echo "${sect}" | sed -n -E 's,^Depends: (.+)$,\1,p')"  # .deb dependencies

        echo "! Version: ${vers}"
        url="${base}/${debu}"
        echo "! Downloading... '${url}'"
        if curl -fsS "${url}" -o pack.bin; then

          hash_fl="$(openssl dgst -sha256 pack.bin \
            | sed -n -E 's,.+= ([0-9a-fA-F]{64}),\1,p')"

          if [ "${hash_fl}" = "${hash}" ]; then
            if ar -x pack.bin data.tar.xz && \
               tar -xf data.tar.xz; then
              subd="$(echo "$(pwd)/usr/${repo}" | sed 's|^mxe-||' | sed 's|x86-64|x86_64|' | sed "s|${HOME}|~|")"
              echo "! OK. Unpacked into: '${subd}'"  # ~/mxe/usr/mxe-x86_64-w64-mingw32.shared
            else
              echo "! Error: Unpacking: '${url}'"
            fi
            rm -f data.tar.xz
          else
            echo "! Error: Verifying package checksum: '${url}'"
            echo "!        Expected: ${hash}"
            echo "!          Actual: ${hash_fl}"
          fi
          rm -f pack.bin

          for i in ${dept//,/}; do
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

mkdir -p "${HOME}/mxe"
(
  cd "${HOME}/mxe" || exit

  # APT root
  base='http://pkg.mxe.cc/repos/apt/debian'

  alias curl='curl -fsS --connect-timeout 15 --retry 3'
  alias gpg='gpg --batch --keyserver-options timeout=15 --keyid-format LONG'

  echo "! Downloading and verifying mxe APT package list..."
  curl -fsS \
    -O "${base}/dists/wheezy/Release.gpg" \
    -O "${base}/dists/wheezy/Release"
  gpg -q --keyserver hkps://keyserver.ubuntu.com --recv-keys 'D43A795B73B16ABE9643FE1AFD8FFF16DB45C6AB'
  gpg --verify-options show-primary-uid-only --verify Release.gpg Release || exit 1
  curl -fsS -O "${base}/dists/wheezy/main/binary-amd64/Packages.gz"
  openssl dgst -sha256 Packages.gz \
  | grep -q "$(sed -E -n 's,^ ([0-9a-fA-F]{64}) [0-9]* main/binary-amd64/Packages.gz$,\1,p' Release)" || exit 1
  gzip -f -d Packages.gz

  done=''

  while [ -n "$1" ]; do
    echo "! Installing mxe package '$1'"
    mxe_get_pkg "$1"
    shift
  done

  echo "! Installed:${done//|/}"
)
