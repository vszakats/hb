#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# for Linux Amazon

set -x; cat /etc/*-release; ulimit -a; df -h

yum install -y gcc git rpm-build findutils \
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
  pcre2-devel \
  postgresql-devel \
  sqlite-devel \
  unixODBC-devel

# qt5-devel: not currently offered
# librabbitmq-devel: version 0.2, too old to be supported
# ocilib-devel: not currently offered
# pcre2-devel: not currently offered

./package/mpkg_rpm.sh

ls -l ./package/RPM/RPMS/x86_64/
