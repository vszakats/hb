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

pass='pass:test'

# Private
privout 'privkey.pem' \
openssl genpkey -algorithm RSA -aes-256-cbc -pkeyopt rsa_keygen_bits:2048 -pass "${pass}"
# human-readable
privout 'privkey.pem.rsa.txt' \
openssl rsa -passin "${pass}" -in 'privkey.pem' -text -noout
privout 'privkey.pem.asn.txt' \
openssl asn1parse             -in 'privkey.pem'

# Public
openssl rsa -passin "${pass}" -in 'privkey.pem' -pubout > pubkey.pem
# human-readable
openssl rsa -passin "${pass}" -in 'pubkey.pem'  -text -noout -pubin > pubkey.pem.txt
