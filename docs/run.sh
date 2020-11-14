#!/bin/sh

cd "$(dirname "$0")" || exit 1

jekyll serve --incremental \
  --ssl-cert localhost-cert.pem \
  --ssl-key  localhost-private.pem \
  --port 3434 \
  --open-url \
  "$@"
