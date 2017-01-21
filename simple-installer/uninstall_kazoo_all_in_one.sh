#!/usr/bin/env bash 

#quick and dirty unintsall script. 



echo "Are you sure you want to stop all serveices and remove the kazoo packages and files? [y|n]"
read $question answer
if ! [[ $answer =~ [yY] ]];then 
    echo "Whew, scared me. Goodbye!"
    exit; 
fi

echo "Stopping Services"

systemctl stop kazoo-applications
systemctl stop kazoo-ecallmgr
systemctl stop kazoo-bigcouch
systemctl stop kazoo-rabbitmq
systemctl stop kazoo-haproxy
systemctl stop kazoo-freeswitch
systemctl stop kazoo-kamailio


echo "Removing Packages"

yum remove -y "monster-ui-*" "kazoo-*" httpd 2600hz-release

echo "removing old directories"

rm -rf /etc/kazoo
rm -rf /opt/kazoo
rm -rf /opt/bigcouch
rm -rf /tmp/*
rm -rf /etc/haproxy
rm -rf /srv
rm -rf /var/log/kamailio
rm -rf /var/log/httpd
rm -rf /var/log/freeswitch
rm -rf /var/log/haproxy.log 
rm -rf /var/log/2600hz-platform.log
rm -rf /var/log/bigcouch
rm /var/log/kazoo_install.log 
rm -rf /etc/httpd
rm -rf /var/lib/rabbitmq
