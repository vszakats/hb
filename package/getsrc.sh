#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2017-present Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Install matching Harbour sources into your existing binary installation.
# Requires: curl, tar

cd "$(dirname "$0")" || exit

curl -fsS -L --proto-redir =https \
  '@HB_URL_SRC@' \
| tar --strip-components 1 -zx

echo 'Sources downloaded and merged into your Harbour directory:'
pwd
