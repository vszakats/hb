#!/bin/sh

case "$(uname)" in
   *Darwin*) alias openssl=/usr/local/opt/openssl/bin/openssl;;
esac

openssl req -new -subj "/O=Example/CN=localhost" -sha256 -newkey rsa:2048 -nodes -keyout privkey.pem -out example.csr
chmod 600 privkey.pem
openssl x509 -req -sha256 -days 730 -in example.csr -signkey privkey.pem -out example.crt

# Human-readable
openssl req       -in example.csr -text -noout > example.csr.txt
openssl asn1parse -in example.csr              > example.csr.asn1.txt

openssl x509      -in example.crt -text -noout > example.crt.txt
openssl asn1parse -in example.crt              > example.crt.asn1.txt

openssl rsa       -in privkey.pem -text -noout > privkey.pem.rsa.txt
chmod 600 privkey.pem.rsa.txt
openssl asn1parse -in privkey.pem              > privkey.pem.asn1.txt
chmod 600 privkey.pem.asn1.txt
