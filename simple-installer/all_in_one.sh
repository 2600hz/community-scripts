#!/bin/bash
set -e
#
## The goal of this script is to deploy Kazoo on a single server.
## For more information, join us on irc: #2600hz@freenode 
#
fUsage () {
    echo "Usage: [--account=account_name_here] [--realm=realm_here] [--user=username_here] [--pass=password_here]"
    echo "Options"
    echo "--account: the name of the user's account"
    echo "--realm: the realm of the user's account"
    echo "--user: the username you use to login"
    echo "--pass: the password you use to login"
    echo "Example:"
    echo " --account=deploy-test --realm=test.sip.2600hz.com --user=test --pass=thisisatest"
    exit 1
}

# Usage if no arguments
if [ -z "$*" ]; then fUsage; fi

# Assign arguments to variables
while [ -n "$*" ]; do
    case "x$1" in
        x--account=*)
            account_name=`echo "$1"|cut -d= -sf2`
            ;;
        x--realm=*)
            realm=`echo "$1"|cut -d= -sf2`
            ;;
        x--user=*)
            user=`echo "$1"|cut -d= -sf2`
            ;;
        x--pass=*)
            pass=`echo "$1"|cut -d= -sf2`
            ;;
        x--help)
            fUsage
            ;;
        *)
            fUsage
            ;;
    esac
    shift
done

## Add colors for easier reading
# Green
green='\e[0;32m'
# Red
red='\e[0;31m'
# no color
NC='\e[0m'

## Debug and error functions so we can read easily the output of the script
debug()  { /bin/echo -e "\n ${green}         # DEBUG: $* ${NC} \n "; }
error()  { /bin/echo -e "\n ${red}         # ERROR: $* ${NC} \n "; }



#checking for fqdn
debug "Getting  FQDN"
hostname -f
fqdn=$(hostname -f)
fqdn_regex="^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])(\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]))*$"

if [[ ! $fqdn =~ $fqdn_regex ]]; then 
    error "Hostname \"$fqdn\" does not seem to be fully qualified"
    exit 1
fi
/bin/echo "Hostname = $fqdn"

#checking for selinux
debug "Checking SELinux"
selinux=`sestatus | awk '{print $NF;}'`
echo "SELinux is currently: $selinux"
if [[ $selinux != "disabled" ]]; then
    error "Please disable SELinux for now, you can of course add it back once you understand how Kazoo works"
    exit 1
fi

# User interaction
cat <<"EOT"
Thanks for flying
         ____   ___   __    __   _  _  ____             
        (___ \ / __) /  \  /  \ / )( \(__  )            
         / __/(  _ \(  0 )(  0 )) __ ( / _/             
        (____) \___/ \__/  \__/ \_)(_/(____)           

This script is community maintained here: https://github.com/2600hz/community-scripts
For more information, join us on irc: #2600hz@freenode 

EOT


#have user select network interface
debug "Getting Network Interfaces Information..."
count=1
echo "Please select which interface you want to configure Kazoo with:"
for i in `ifconfig | grep Ethernet| awk '{print $1}'`
do  
    tmpIP=`ifconfig $i | grep "inet addr" | cut -d: -f 2 | awk '{ print $1}'`
    echo "$count) $i $tmpIP"
    count=$(($count+1))
done
read answer
count=1
for i in `ifconfig | grep Ethernet| awk '{print $1}'`
do
    case $answer in
        $count) ip_address=`ifconfig $i | grep "inet addr" | cut -d: -f 2 | awk '{ print $1}'`
            ;;      
    esac
    count=$(($count+1))
done
if [ -z "$ip_address" ]; then 
     error "Invalid interface selection";
    exit 1
fi
echo "IP = $ip_address"

#Prerequisites
debug "Installing Prerequisites Packages"
yum install -y sed curl rsyslog httpd

#Installing Softwares
debug "Configuring 2600Hz Repository"
curl -o /etc/yum.repos.d/2600hz.repo http://repo.2600hz.com/2600hz.repo
yum clean all
debug "Installing 2600Hz Packages"
yum install -y esl-erlang kazoo-R15B kazoo-kamailio haproxy kazoo-ui kazoo-freeswitch-R15B kazoo-bigcouch-R15B

#Configuring HAProxy
haproxy_cfg='/etc/haproxy'
if [ ! -L $haproxy_cfg ]; then
  debug "Replace /etc/haproxy with symlink /etc/kazoo/haproxy"
  /bin/rm -rf /etc/haproxy/haproxy.cfg
  /bin/ln -s /etc/kazoo/haproxy/haproxy.cfg /etc/haproxy/haproxy.cfg
fi

##Configuring FreeSWITCH
## NOTE: This is not required, if shortnames are set to false 
##    and the node name does not include the servers FQDN then
##    mod_kazoo will get it from the system when it starts.
##    This is better than hard-coding it because it makes it "just work"
##    if the hostname is ever changed.
## debug "Edit freeswitch nodename"
## /bin/sed -i s/ue=\"freeswitch\"/ue=\"freeswitch@$fqdn\"/g /etc/kazoo/freeswitch/autoload_configs/kazoo.conf.xml

debug "Configuring Kamailio dispatcher"
/bin/sed -i s/127.0.0.1/$ip_address/g /etc/kazoo/kamailio/dbtext/dispatcher
/bin/sed -i s/127.0.0.1/$ip_address/g /etc/kazoo/kamailio/local.cfg
/bin/cat /etc/kazoo/kamailio/dbtext/dispatcher | grep "sip:$ip_address"

debug "Show Kazoo config.ini"
/bin/cat /etc/kazoo/config.ini

#Start/restart services
debug "Restart Rsyslog"
/etc/init.d/rsyslog restart

debug "Start EPMD"
/usr/bin/epmd -daemon
/bin/netstat -ptlen | grep epmd

debug "Restart HAProxy to apply changes"
/etc/init.d/haproxy start

debug "Start RabbitMQ"
/etc/init.d/rabbitmq-server start

debug "Start FreeSWITCH"
/etc/init.d/freeswitch stop
#use init script?
/usr/bin/freeswitch -nc -nonat &

debug "Restart Kamailio to apply the changes"
/etc/init.d/kamailio restart

debug "Start Whapps"
/etc/init.d/kz-whistle_apps start

debug "Start ecallmgr"
/etc/init.d/kz-ecallmgr start

debug "Wait for 30s and check on Whapps & ecallmgr status"
/bin/sleep 30
/etc/init.d/kz-whistle_apps status

debug "Attach ecallmgr to FreeSWITCH"
/opt/kazoo/utils/sup/sup -n ecallmgr ecallmgr_maintenance add_fs_node freeswitch@$fqdn
#add sup command SBC?

#Kazoo-UI configurations
debug "Edit config.js with actual IP address"
/bin/sed -i s/'api.2600hz.com'/$ip_address/g /var/www/html/kazoo-ui/config/config.js

debug "Show change of IP"
/bin/cat /var/www/html/kazoo-ui/config/config.js | grep api_url

debug "Start Apache"
/etc/init.d/httpd start

debug "Importing media files"
/opt/kazoo/utils/media_importer/media_importer /opt/kazoo/system_media/*.wav 

#Create new account
debug "Create test account"
/opt/kazoo/utils/sup/sup crossbar_maintenance create_account $account_name $realm $user $pass

debug "Congratulations, your server is now running an All-in-One Kazoo, you can access the web interface by pointing your browser to: http://$ip_address/kazoo-ui"

exit
