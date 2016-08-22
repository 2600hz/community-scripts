# generate a selfsigned certificate and private key
openssl req -x509 -nodes -days 3650 -newkey rsa:4096 \
   -config ./openssl-management-proxy.cnf \
   -keyout ./management-proxy.key \
   -out ./management-proxy.crt
