# Management Proxy

Access Kazoo's management tools from one site: 
<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;      CouchDB's Fauxton,&nbsp;&nbsp;  RabbitMQ's management plugin&nbsp;&nbsp;  and&nbsp;&nbsp;  HAProxy's status page.
<br />
  
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
<br />
For other distros/OSs, you will have to manually install Nginx, then run the script.
<br />
  
## \-\-\-\-\- Notes \-\-\-\-\-

You can also access management tools which reside on separate servers.
<br />
For instance, if CouchDB is hosted on a different server, change line:
<br />
&nbsp;&nbsp;&nbsp;   `proxy_pass http://127.0.0.1:5984/$1;`
<br />
to
<br />
&nbsp;&nbsp;&nbsp;   `proxy_pass http://COUCHDB-IP-ADDRESS:5984/$1;`
<br />
  
## \-\-\-\-\- Security \-\-\-\-\-

The proxy is configured to listen to port 443 on all interfaces using IPv4 and IPv6.
<br />
The golden rule of security is **"If you don't use it, disable it"**.
<br />
We suggest limiting the proxy to localhost:443 and using an SSH-tunnel to access the management page.
<br />
If you must expose the proxy to the public Internet, add password protection to the config file. 
<br />
We advise against relying solely on firewall rules.
<br />
  
<br />
  
Sponsored by GBC Networks Oy (http://gbc.fi)
