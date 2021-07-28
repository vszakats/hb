#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2016-present Viktor Szakats
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Extract dependency versions and their hashes from `curl-for-win`
# online build log.
#
# Requires: bash, curl, jq, awk

readonly ci="${1:-appveyor}"
readonly username='curlorg'
readonly project='curl-for-win'
readonly branch='main'

echo "! CI: ${ci}"
echo "! Project: ${project}"
echo "! Branch: ${branch}"

case "${ci}" in
  appveyor)
    # https://www.appveyor.com/docs/api/

    # Query branch build state for a successfully finished job
    # .build.buildNumber
    f="$(curl -fsS "https://ci.appveyor.com/api/projects/${username}/${project}/branch/${branch}" 2> /dev/null)"
    jobid="$(echo "${f}" \
      | jq -r 'select(.build.jobs[0].status == "success") | .build.jobs[0].jobId')"
    bldid="$(echo "${f}" \
      | jq -r '.build.buildNumber')"
    ;;
esac

if [ -n "${jobid}" ] && [ ! "${jobid}" = 'null' ]; then

  echo "! Build number: ${bldid}"

  unset GREP_OPTIONS

  case "${ci}" in
    appveyor)
      bhost='windows'
      f="$(curl -fsS "https://ci.appveyor.com/api/buildjobs/${jobid}/log" | grep 'SHA256(')"
      ;;
  esac

  if [ -n "${jobid}" ]; then

    echo "! Job Id: ${jobid} ${jobid2}"
    echo "! Host OS: ${bhost}"

    out=

    for name in \
       'brotli' \
       'nghttp2' \
       'openssl' \
       'libssh2' \
       'curl' \
    ; do
      nameu="$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)"
      for plat in '32' '64'; do
        if [[ "${f}" =~ ${name}-([0-9a-zA-Z.-]+)-win${plat}-mingw\.7z\)=\ ([0-9a-z]{64}) ]]; then
          if [ "${plat}" = '32' ]; then
            out="${out}export ${nameu}_VER='${BASH_REMATCH[1]}'"$'\n'
          fi
          out="${out}export ${nameu}_HASH_${plat}='${BASH_REMATCH[2]}'"$'\n'
        fi
      done
    done

    if [ -n "${out}" ]; then
      # remove ending EOL
      # shellcheck disable=SC2116
      out="$(echo "${out}")"
      echo "${out}"
      # shellcheck disable=SC1117
      awk -v "NEW=#hashbegin\n${out}\n#hashend" \
        'BEGIN{n=0} /#hashbegin/ {n=1} {if (n==0) {print $0}} /#hashend/ {print NEW; n=0}' \
        < mpkg_win_ci.sh > _tmp && cp _tmp mpkg_win_ci.sh && rm -f _tmp

      git diff ./mpkg_win_ci.sh
    else
      echo '! Error: Hashes not found. Incomplete or non-production build?'
    fi
  else
    echo '! Error: Last job failed or not finished yet.'
  fi
else
  echo '! Error: Last build/job failed or not finished yet.'
fi
