#!/usr/bin/env bash

set -e
set -o nounset

lockfile="/var/lock/subsys/check_interfaces.lockfile"

. /opt/kazoo_install/setup_common

trap clean_exit SIGHUP SIGTERM ERR EXIT
trap int_exit SIGINT SIGQUIT

log_file=/var/log/check_interfaces.log

datestamp=date +"%F %T"


info " : Setting up kazoo on-boot addresses : "

dbg " Kazoo IP Configuration "
    
if [[ ${1:-} =~ -a ]];then
    all_in_one=1
fi


check_root
check_lock
    
system_hostname=""; get_system_hostname
system_ip_address=""; ask_ip_selection


ip_address="";
check_info_file

declare -a ip_addresses
declare -a interfaces

get_interfaces
get_system_ip 

ip_match=""

for ip_addr in ${ip_addresses[@]}; do
	if [[ "$ip_addr" == "$system_ip_address" ]];then
		ip_match=1
	fi			
done



    
else
    error "No IP addresses could be found, please setup your interfaces or this server won't work!" 
fi


if ! [[ ${ip_match:-} ]];then 
    if [[ ${#ip_addresses[@]:-} -eq 1 ]];then
	    setup_packages -a -i kamailio   
	elif [ ! ${#ip_addresses[@]:-} -gt 1 ];then 
	    echo 'ask_ip_address' >> /home/root/.bashrc
	fi
fi


	
clean_exit $lockfile




