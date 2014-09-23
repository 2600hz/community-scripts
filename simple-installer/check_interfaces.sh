#!/usr/bin/env bash

set -e
set -o nounset

lockfile="/var/lock/subsys/check_interfaces.lockfile"

. /opt/kazoo_install/setup_common

trap clean_exit SIGHUP SIGTERM ERR EXIT
trap int_exit SIGINT SIGQUIT

log_file=/var/log/check_interfaces.log

datestamp=date +"%F %T"

system_ip_address=""; ask_ip_selection

interactive(){
    ask admin_user
    ask ip_address
	ask admin_realm  
	ask admin_password
}

if [[ -e /etc/kazoo/kazoo_boot.conf ]]; then
	ip_address=`sed -n "s|IP_ADDRESS=\(.*\)|\1|g" /etc/kazoo/kazoo_boot.conf`
	admin_user= sed -n "s|ADMIN_USER=\(.*|\1\)|g" /etc/kazoo/kazoo_boot.conf`
	admin_passowrd=`sed -n "s|ADMIN_PASSWORD=\(.*\)|\1|g" /etc/kazoo/kazoo_boot.conf`
	admin_realm=`sed -n "s|ADMIN_REALM=\(.*\)|\1|g" /etc/kazoo/kazoo_boot.conf`
fi


info " : checking Kazoo IP : "
sed -i "s|IP_ADDRESS=.*|IP_ADDRESS=$ip_addr|g" /etc/kazoo/kazoo_boot.conf
sed -i "s|ADMIN_USER=.*|ADMIN_USER=$ip_addr|g" /etc/kazoo/kazoo_boot.conf
sed -i "s|ADMIN_PASSWORD=.*|ADMIN_PASSWORD=$ip_addr|g" /etc/kazoo/kazoo_boot.conf
sed -i "s|ADMIN_REALM=.*|ADMIN_REALM=$ip_addr|g" /etc/kazoo/kazoo_boot.conf

set_value cookie $default_cookie

if [[ ${1:-} =~ -a ]];then
    all_in_one=1
fi

if [  ${all_in_one:-} ];then
   #if allinone we default everything we can
   amqp_string="guest:guest@127.0.0.1:5672"
   ip_address="127.0.0.1"
else
   info "NOTE: You can type '${NC}?${blue}' in response to any question for a hint"
   interactive
fi

dbg "Applying Kazoo configuration..."

set_value amqp_string $amqp_string
set_value cookie $cookie
set_value ip_address $ip_address

clean_exit $lockfile




