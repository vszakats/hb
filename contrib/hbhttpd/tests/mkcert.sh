#!/bin/sh

case "$(uname)" in
   *Darwin*) alias openssl=/usr/local/opt/openssl/bin/openssl;;
esac

openssl req -new -subj "/C=LT/O=My Company/CN=localhost" -sha256 -newkey rsa:2048 -nodes -keyout privatekey.pem -out certrequest.csr
chmod 600 privatekey.pem
openssl x509 -req -sha256 -days 730 -in certrequest.csr -signkey privatekey.pem -out certificate.pem

openssl req       -in certrequest.csr -text -noout > certrequest.csr.txt
openssl x509      -in certificate.pem -text -noout > certificate.pem.txt
openssl asn1parse -in privatekey.pem               > privatekey.pem.asn.txt
chmod 600 privatekey.pem.asn.txt
openssl rsa       -in privatekey.pem  -text -noout > privatekey.pem.rsa.txt
chmod 600 privatekey.pem.rsa.txt
