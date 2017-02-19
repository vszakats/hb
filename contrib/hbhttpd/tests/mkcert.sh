#!/bin/sh

# Create a self-signed certificate for localhost/loopback

case "$(uname)" in
   Darwin*) alias openssl=/usr/local/opt/openssl/bin/openssl;;
esac

tmp="$(mktemp -t XXXXXX)"

cat << EOF > "${tmp}"
[req]
encrypt_key = no
prompt = no
distinguished_name = dn
req_extensions = v3_req

[dn]
O = Example
CN = localhost

[v3_req]
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
DNS.2 = ::1
DNS.3 = 127.0.0.1
EOF

openssl req -new -sha256 \
  -config "${tmp}" \
  -newkey rsa:2048 -nodes -keyout private.pem -out example.csr
chmod 600 private.pem

openssl x509 -req -sha256 -days 730 \
  -extfile "${tmp}" -extensions v3_req \
  -in example.csr -signkey private.pem -out example.crt
rm "${tmp}"

# Human-readable
openssl req       -in example.csr -text -noout > example.csr.txt
openssl asn1parse -in example.csr              > example.csr.asn1.txt

openssl x509      -in example.crt -text -noout > example.crt.txt
openssl asn1parse -in example.crt              > example.crt.asn1.txt

openssl rsa       -in private.pem -text -noout > private.pem.rsa.txt
chmod 600 private.pem.rsa.txt
openssl asn1parse -in private.pem              > private.pem.asn1.txt
chmod 600 private.pem.asn1.txt
