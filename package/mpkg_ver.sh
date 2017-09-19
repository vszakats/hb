#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# small set of functions used by Harbour scripts
#
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

hb_get_ver() {
  verfile='../include/hbver.h'
  ver_maj="$(sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${verfile}")"
  ver_min="$(sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${verfile}")"
  ver_rel="$(sed -e '/HB_VER_RELEASE/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${verfile}")"
  echo "${ver_maj}.${ver_min}.${ver_rel}"
}

hb_get_ver_majorminor() {
  verfile='../include/hbver.h'
  ver_maj="$(sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${verfile}")"
  ver_min="$(sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${verfile}")"
  echo "${ver_maj}.${ver_min}"
}

hb_get_ver_status() {
  verfile='../include/hbver.h'
  ver_sta="$(sed -e '/HB_VER_STATUS/ !d' -e 's/[^\"]*\"\([^\"]*\).*/\1/g' "${verfile}")"
  echo "${ver_sta}"
}
