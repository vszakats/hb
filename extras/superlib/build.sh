#!/bin/sh

curl -O https://harbour.github.io/the-oasis/files/library/supfree.zip
openssl dgst -sha256 supfree.zip | grep -q '48114a59c8a9ebf51749f74e31dac39e12403180ae2920afb9f93d82794a8fc6' || exit 1

unzip -qo supfree.zip SOURCE.ZIP SAMPLES.ZIP
unzip -qo SOURCE.ZIP

hbmk2 -sanitize *.C *.PRG
patch -li superlib.patch

hbmk2 superlib.hbp

unzip -qo SAMPLES.ZIP
hbmk2 S3PROG.PRG superlib.hbc -run
