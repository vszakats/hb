#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2016-2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Extract dependency versions and their hashes from `harbour-deps` online build log
# Requires: bash, curl, jq, awk

readonly ci="${1:-appveyor}"
readonly project='harbour-deps'
branch='master'

echo "! CI: ${ci}"
echo "! Project: ${project}"
echo "! Branch: ${branch}"

case "${ci}" in
  appveyor)
    # https://www.appveyor.com/docs/api/

    # Query branch build state for a successfully finished job
    # .build.buildNumber
    f="$(curl -fsS "https://ci.appveyor.com/api/projects/vsz/${project}/branch/${branch}" 2> /dev/null)"
    jobid="$(echo "${f}" \
      | jq -r 'select(.build.jobs[0].status == "success") | .build.jobs[0].jobId')"
    bldid="$(echo "${f}" \
      | jq -r '.build.buildNumber')"
    ;;
  travis)
    # https://docs.travis-ci.com/api

    # Query for a finished or running branch build state and extract job id
    # job[0]: linux, job[1]: osx
    f="$(curl -fsS "https://api.travis-ci.org/repos/vszakats/${project}/branches/${branch}" 2> /dev/null)"
    jobid="$(echo "${f}" \
      | jq -r 'select(.branch.state | test("(started|finished|passed|restarted)")) | .branch.job_ids[1]')"
    bldid="$(echo "${f}" \
      | jq -r '.branch.number')"
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
    travis)
      # Query for a successfully finished job
      f="$(curl -fsS "https://api.travis-ci.org/jobs/${jobid}")"
      bldid="$(echo "${f}" | jq -r '.number')"
      bhost="$(echo "${f}" | jq -r '.config.os')"
      if [ "$(echo "${f}" | jq -r '.state')" = 'finished' ] && \
         [ "$(echo "${f}" | jq -r '.result')" = '0' ]; then
        # Download log
        f="$(curl -fsS -L --proto-redir =https "https://api.travis-ci.org/jobs/${jobid}/log" | grep 'SHA256(')"
      else
        unset jobid
      fi
      ;;
  esac

  if [ -n "${jobid}" ]; then

    echo "! Job Id: ${jobid}"
    echo "! Host OS: ${bhost}"

    out=

    for name in \
       'nghttp2' \
       'openssl' \
       'libssh2' \
       'curl' \
    ; do
      nameu="$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)"
      for plat in '32' '64'; do
        if [[ "${f}" =~ ${name}-([0-9a-zA-Z.\-]+)-win${plat}-mingw.7z\)=\ ([0-9a-z]{64}) ]]; then
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
      awk -v "NEW=#hashbegin\n${out}\n#hashend" \
        'BEGIN{n=0} /#hashbegin/ {n=1} {if (n==0) {print $0}} /#hashend/ {print NEW; n=0}' \
        < mpkg_win_ci.sh > _tmp && cp _tmp mpkg_win_ci.sh
      rm _tmp
    else
      echo '! Error: Hashes not found. Incomplete or non-production build?'
    fi
  else
    echo '! Error: Last job failed or not finished yet.'
  fi
else
  echo '! Error: Last build/job failed or not finished yet.'
fi
