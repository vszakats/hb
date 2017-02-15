#!/bin/bash

# Copyright 2014-2017 Viktor Szakats (vszakats.net/harbour)
# The MIT License (MIT)

# Fetch all http URLs from all text files and check whether the same URL is
# available via HTTPS.

# REQUIRES: grep, curl (built with SSL)
# CAVEAT: on some systems, curl may accept a different set of certificates
#         than web browsers.

curl_req()
{
  cookies="$(mktemp -t XXXXXXXX)"
  curl -fsS \
    -L --max-redirs 10 \
    --cookie-jar "${cookies}" \
    --connect-timeout 5 \
    --user-agent Mozilla "$1" 2> /dev/null
  rm -f ${cookies}
}

urls='./_url'

if [ ! -f "${urls}" ]; then
  export GREP_OPTIONS=
  grep -RHEIno '(http)://[a-zA-Z0-9_/\.\~\%\?&\+=@:-]*' * > "${urls}"
fi

ignored=0
upgradable=0
possible=0
remaining=0
broken=0

while IFS= read -r _line; do
  # input line format (grep output-like): filename:linenumber:url
  if [[ "${_line}" =~ ^([^:]*):([^:]*):(http):(.*$) ]]; then

    fn="${BASH_REMATCH[1]}"  # filename
    ln="${BASH_REMATCH[2]}"  # line number
    pr="${BASH_REMATCH[3]}"  # protocol
    ad="${BASH_REMATCH[4]}"  # //address

    if [ -n "${ad}" ] && \
       [[ "${ad}" != *"apple.com/DTDs/"* ]] && \
       [[ "${ad}" != *"googlecode.com/svn/"* ]] && \
       [[ "${ad}" != *"svn.code.sf.net/"* ]] && \
       [[ "${ad}" != *"/trunk"* ]] && \
       [[ "${ad}" != *"/svn."* ]] && \
       [[ "${ad}" != *".xsd" ]] && \
       [ "${ad}" != '//' ] && \
       [[ "/${fn}" != *"/vendor/"* ]] && \
       [[ "/${fn}" != *"/3rd/"* ]]; then

      # Read whole line where the url was found
      lns="$(sed "${ln}q;d" "${fn}" 2> /dev/null)"

      if [[ "${ad}" != *"xmlns"* ]]; then
        if [ "${pr}" = 'http' ]; then
          outs="$(curl_req "https:${ad}")"
          outp="$(curl_req "http:${ad}")"
          if [ -z "${outs}" ]; then
            if [ -z "${outp}" ]; then
              echo "${fn}:${ln} http:${ad} [Possibly lost link]"
              ((broken++))
            else
              ((remaining++))
            fi
          else
            if [ "${outs}" == "${outp}" ]; then
              ((upgradable++))
              hint='OK'
            else
              ((possible++))
              hint='Different content - verify manually'
            fi
            echo "${fn}:${ln} -> https:${ad} [${hint}]"
          fi
        else
          ((ignored++))
        fi
      else
        ((ignored++))
      fi
    else
      ((ignored++))
    fi
  fi
done < "${urls}"

echo
echo '--- SUMMARY'
echo "Upgradable to HTTPS: ${upgradable}"
echo "Flagged for manual HTTPS check: ${possible}"
echo "Remains HTTP for now: ${remaining}"
echo "Possibly broken link: ${broken}"
