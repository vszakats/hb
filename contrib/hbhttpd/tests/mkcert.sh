#!/bin/sh

openssl req -new -subj "/C=LT/O=My Company/CN=localhost" -sha256 -newkey rsa:2048 -nodes -keyout privatekey.pem -out certrequest.csr
chmod 600 privatekey.pem
openssl x509 -req -sha256 -days 730 -in certrequest.csr -signkey privatekey.pem -out certificate.pem

openssl req  -in certrequest.csr -text > certrequest.csr.txt
openssl x509 -in certificate.pem -text > certificate.pem.txt
