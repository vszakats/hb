#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# for Linux Ubuntu Yakkety

set -x

cat /etc/*-release
ulimit -a
df -h

dpkg --add-architecture i386
apt-get -qq update
apt-get -qq install \
  curl dirmngr git make gcc binutils \
  binutils-mingw-w64 gcc-mingw-w64 g++-mingw-w64 p7zip-full dos2unix realpath wine-stable wine64 wine32

echo 'deb http://pkg.mxe.cc/repos/apt/debian wheezy main' > /etc/apt/sources.list.d/mxeapt.list
curl -fsS --connect-timeout 15 --retry 3 'https://keyserver.ubuntu.com/pks/lookup?search=0xD43A795B73B16ABE9643FE1AFD8FFF16DB45C6AB&op=get' \
| gpg --import --status-fd 1
gpg --export D43A795B73B16ABE9643FE1AFD8FFF16DB45C6AB \
| apt-key add -
apt-get -qq update
apt-get -qq install \
  mxe-{i686,x86-64}-w64-mingw32.shared-{cairo,file,ghostscript,icu4c,libmysqlclient,postgresql} \
  mxe-{i686,x86-64}-w64-mingw32.static-{freeimage,gd}

./package/mpkg_win_ci.sh
