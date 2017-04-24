#!/bin/sh

curl -LO --proto-redir =https https://web.archive.org/web/abeelabs.com/downloads/PS32.zip
openssl dgst -sha256 PS32.zip | grep -q 'c9a9fb6d81054468d2884a913275184a318be8a5c830bc901376ae88e720849d' || exit 1

unzip -qoj PS32.zip \
  'PS32/xHarbour/PScript.ch' \
  'PS32/xHarbour/TPSCRIPT.PRG' \
  'PS32/Demos/xbase sources/PSTest.prg'

hbmk2 -sanitize *.ch *.prg *.PRG

patch -li ps32.patch

hbmk2 ps32.hbp
hbmk2 pstest.prg ps32.hbc
