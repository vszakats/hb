#!/bin/sh

# This script requires 'GNU tar'.

if [ "$1" = 'zip' ] || [ "$1" = 'ZIP' ]; then
  hb_archbin='zip'
  hb_ext='.zip'
elif gtar --version >/dev/null 2>&1; then
  hb_archbin='gtar'
elif tar --version >/dev/null 2>&1; then
  hb_archbin='tar'
else
  hb_archbin='tar'
  echo "Warning!!! Cannot find 'GNU tar'"
fi
[ -n "${hb_ext}" ] || hb_ext='.tar.gz'

hb_currdir="$(pwd)"

if [ -f mpkg_ver.sh ]; then
  hb_rootdir='..'
else
  hb_rootdir=$(dirname "$0")
  hb_rootdir=$(dirname "${hb_rootdir}")
fi

# shellcheck source=./mpkg_ver.sh
. "${hb_rootdir}/package/mpkg_ver.sh"
hb_verfull=$(hb_get_ver)

hb_filename="${hb_currdir}/harbour-${hb_verfull}.src${hb_ext}"
rm -f "${hb_filename}"

#[ -z "${TZ}" ] && export TZ=UTC

hb_collect_all_git() {
  for f in $(git ls-tree HEAD -r --name-only); do
    [ -f "${f}" ] && echo "${f}"
  done
}

hb_collect_all_tree() {
  unset GREP_OPTIONS
  _exclude='/obj/|/lib/|/bin/.*/|\.tar|\.zip|\.exe|\.log|/linux/|/win|/config/'
  for f in $(find . -type f | grep -vE "${_exclude}"); do
    echo "${f}" | awk '{ string=substr($0, 2); print string; }'
  done
  find config -type f -exec echo '{}' \;
}

hb_flist='bin/hb_flist.tmp'
(
  cd "${hb_rootdir}" || exit
  if [ -d '.git' ]; then
    hb_collect_all_git
  else
    hb_collect_all_tree
  fi
) | LC_ALL=C sort > "${hb_rootdir}/${hb_flist}"

(
  cd "${hb_rootdir}" || exit
  if [ "${hb_archbin}" = 'zip' ]; then
    ${hb_archbin} -X -9 -o -r -q "${hb_filename}" . "-i@${hb_flist}"
  else
    # https://lists.gnu.org/archive/html/help-tar/2015-05/msg00005.html
    ${hb_archbin} -c --files-from "${hb_flist}" \
      --owner=0 --group=0 --numeric-owner \
      --mode=go=rX,u+rw,a-s \
    | gzip -9 -n > "${hb_filename}"
  fi
)
rm -f "${hb_rootdir:?}/${hb_flist}"

cd "${hb_currdir}" || exit
