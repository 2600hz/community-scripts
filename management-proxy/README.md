# Management Proxy

Access Kazoo's management tools from one site: CouchDB's Fauxton, RabbitMQ's management plugin and HAProxy's status page.


## \-\-\-\-\- Installation \-\-\-\-\-

#### Install
```bash
cd management-proxy
chmod +x install-management-proxy.sh
sudo ./install-management-proxy.sh
```
#### Test
```bash
lynx https://localhost:443
```
The installer works on Debian, RedHat/CentOS and Solaris. 
For other distros/OSs, you will have to manually install Nginx, then run the script.

## \-\-\-\-\- Notes \-\-\-\-\-

You can also access management tools which reside on separate servers.
For instance, if CouchDB is hosted on a different server, change line:
`proxy_pass http://127.0.0.1:5984/$1;`
to
`proxy_pass http://COUCHDB-IP-ADDRESS:5984/$1;`


## \-\-\-\-\- Security \-\-\-\-\-

The proxy is configured to listen to port 443 on all interfaces using IPv4 and IPv6.
The golden rule of security is **"If you don't use it, disable it"**.
We suggest limiting the proxy to localhost:443 and using a SSH-tunnel to access the management page.
If you must expose the proxy to the public Internet, add password protection to the config file. We advise against relying solely on firewall rules.
<br />
Sponsored by GBC Networks Oy (http://gbc.fi)
