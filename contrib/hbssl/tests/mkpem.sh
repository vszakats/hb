#!/bin/sh

# Copyright 2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.

# Generate RSA keypair with encrypted private key
# Requires: OpenSSL 1.x or upper
#           (install with `brew install openssl` on Mac)

case "$(uname)" in
   *Darwin*)
      openssl() {
         /usr/local/opt/openssl/bin/openssl "$@"
      };;
esac

privout() {
   o="$1"; rm -f "$o"; touch "$o"; chmod 0600 "$o"; shift; "$@" >> "$o"
}

readonly pass='pass:test'

# Private
privout 'private.pem' \
openssl genpkey -algorithm RSA -aes-256-cbc -pkeyopt rsa_keygen_bits:2048 -pass "${pass}"
# human-readable
privout 'private.pem.asn1.txt' \
openssl asn1parse             -in 'private.pem'

# Public
openssl rsa -passin "${pass}" -in 'private.pem' -pubout > public.pem
# human-readable
openssl rsa -pubin            -in 'public.pem'  -text -noout > public.pem.txt
