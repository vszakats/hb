#!/bin/sh

curl -O https://harbour.github.io/the-oasis/files/library/supfree.zip
openssl dgst -sha256 supfree.zip | grep -q '48114a59c8a9ebf51749f74e31dac39e12403180ae2920afb9f93d82794a8fc6' || exit 1

unzip supfree.zip
unzip SOURCE.ZIP

hbmk2 -sanitize '*.C' '*.PRG'
patch -lNi superlib.patch

hbmk2 superlib.hbp
hbmk2 test.prg superlib.hbc -run
