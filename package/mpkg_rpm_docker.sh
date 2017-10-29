#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# for Linux: Amazon

set -x; cat /etc/*-release; ulimit -a; df -h

# librabbitmq-devel: version 0.2, too old to be supported
# not currently offered: firebird-devel freeimage-devel libmariadb-devel ocilib-devel pcre2-devel
# not offered: qt5-devel

yum install -y gcc64 git rpm-build findutils \
  bzip2-devel \
  cairo-devel \
  cups-devel \
  curl-devel \
  expat-devel \
  file-libs \
  firebird-devel \
  freeimage-devel \
  gd-devel \
  ghostscript-devel \
  libicu-devel \
  libmariadb-devel \
  libpng-devel \
  libyaml-devel \
  lzo-devel \
  minizip-devel \
  openssl-devel \
  ocilib-devel \
  pcre-devel \
  pcre2-devel \
  postgresql-devel \
  qt5-devel \
  sqlite-devel \
  unixODBC-devel

time ./package/mpkg_rpm.sh

ls -l ./package/RPM/RPMS/x86_64/
