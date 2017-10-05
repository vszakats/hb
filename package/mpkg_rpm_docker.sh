#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# for Amazon Linux

set -x; cat /etc/*-release; ulimit -a; df -h

yum install -y gcc64 git rpm-build findutils \
  bzip2-devel \
  cairo-devel \
  cups-devel \
  curl-devel \
  expat-devel \
  file-libs \
  firebird-devel \     # not currently offered
  freeimage-devel \    # not currently offered
  gd-devel \
  ghostscript-devel \
  libicu-devel \
  libmariadb-devel \   # not currently offered
  libpng-devel \
  libyaml-devel \
  lzo-devel \
  minizip-devel \
  openssl-devel \
  ocilib-devel \       # not currently offered
  pcre-devel \
  pcre2-devel \        # not currently offered
  postgresql-devel \
  qt5-devel \          # not offered
  sqlite-devel \
  unixODBC-devel

# librabbitmq-devel: version 0.2, too old to be supported

./package/mpkg_rpm.sh

ls -l ./package/RPM/RPMS/x86_64/
