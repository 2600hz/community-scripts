#!/bin/bash

## This script is a draft. The goal is to deploy Kazoo v3.0

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
debug() { /bin/echo -e "\n ${green} # DEBUG: $* ${NC} \n "; }
error() { /bin/echo -e "\n ${red} # ERROR: $* ${NC} \n "; }



#checking for fqhn
debug "Getting FQDN"
/bin/hostname -f
fqhn=$(hostname -f)
fqhn_regex="^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])(\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]))*$"

if [[ ! $fqhn =~ $fqhn_regex ]]; then
error "Hostname \"$fqhn\" does not seem to be fully qualified"
    exit 1
fi
/bin/echo "Hostname = $fqhn"

debug "Configure Kazoo Repo"
curl -o /etc/yum.repos.d/2600hz.repo http://repo.2600hz.com/2600hz.repo

yum clean all

debug "Yum installing all necessary RPMs"
yum install -y esl-erlang kazoo-R15B kazoo-kamailio haproxy rsyslog httpd kazoo-ui kazoo-freeswitch-R15B kazoo-bigcouch-R15B sed


#check for all necessary RPMs
debug "Checking for required RPMs"


rpmcheckcount=1

rpmcheckkazoo=`rpm -qa | grep kazoo-R15B`
if [[ ! $rpmcheckkazoo == *15B* ]]; then
error "Missing or wrong version of Kazoo!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckerlang=`rpm -qa | grep erlang`
if [[ ! $rpmcheckerlang == *R15B03* ]]; then
error "Missing or wrong version of Erlang!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckkamailio=`rpm -qa | grep kamailio`
if [[ ! $rpmcheckkamailio == *kamailio* ]]; then
error "Missing or wrong version of kamailio!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckhaproxy=`rpm -qa | grep haproxy`
if [[ ! $rpmcheckhaproxy == *haproxy* ]]; then
error "Missing or wrong version of haproxy!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckcouch=`rpm -qa | grep bigcouch`
if [[ ! $rpmcheckcouch == *couch* ]]; then
error "Missing or wrong version of bigcouch!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckfreeswitch=`rpm -qa | grep freeswitch`
if [[ ! $rpmcheckfreeswitch == *freeswitch* ]]; then
error "Missing or wrong version of freeswitch!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckkazooui=`rpm -qa | grep kazoo-ui`
if [[ ! $rpmcheckkazooui == *kazoo-ui* ]]; then
error "Missing or wrong version of Kazoo-UI!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckhttpd=`rpm -qa | grep httpd`
if [[ ! $rpmcheckhttpd == *httpd* ]]; then
error "Missing or wrong version of httpd!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

rpmcheckrsyslog=`rpm -qa | grep rsyslog`
if [[ ! $rpmcheckrsyslog == *rsyslog* ]]; then
error "Missing or wrong version of rsyslog!"
    rpmcheckcount=$(($rpmcheckcount+1))
fi

if [[ ! $rpmcheckcount == 1 ]]; then
error "Go back and fix the aforementioned packages!"
    exit 1
fi

debug "Found all required RPMs"

#set external ip

ip_adress=$(hostname -i)

/bin/echo "IP = $ip_address"

#checking for selinux
debug "Checking selinux"
selinux=`grep =enforcing /etc/selinux/config`
if [ $selinux ]; then
error "Please disable selinux before continuing"
    exit 1
fi



haproxy_cfg='/etc/haproxy'
if [ ! -L $haproxy_cfg ]; then
debug "Replace /etc/haproxy with symlink /etc/kazoo/haproxy"
  /bin/rm -rf /etc/haproxy/haproxy.cfg
  /bin/ln -s /etc/kazoo/haproxy/haproxy.cfg /etc/haproxy/haproxy.cfg
fi

debug "Edit freeswitch nodename"
/bin/sed -i s/ue=\"freeswitch\"/ue=\"freeswitch@$fqhn\"/g /etc/kazoo/freeswitch/autoload_configs/kazoo.conf.xml

debug "Edit kamailio dispatcher"
/bin/sed -i s/127.0.0.1/$ip_address/g /etc/kazoo/kamailio/dbtext/dispatcher
/bin/sed -i s/127.0.0.1/$ip_address/g /etc/kazoo/kamailio/local.cfg

debug "Show changes"
/bin/cat /etc/kazoo/kamailio/dbtext/dispatcher | grep "sip:$ip_address"


#Start/restart services

debug "Start EPMD"
/usr/bin/epmd -daemon
/bin/netstat -ptlen | grep epmd

debug "Start FreeSWITCH"
/etc/init.d/freeswitch stop
/usr/bin/freeswitch -nc -nonat &

debug "Restart HAProxy to apply changes"
/etc/init.d/haproxy start

debug "Restart Kamailio to apply the changes"
/etc/init.d/kamailio restart

debug "Start RabbitMQ"
/etc/init.d/rabbitmq-server start

debug "Show kazoo config.ini"
/bin/cat /etc/kazoo/config.ini

debug "Start Whapps"
/etc/init.d/kz-whistle_apps start

debug "Start ecallmgr"
/etc/init.d/kz-ecallmgr start

debug "Restart rsyslog"
/etc/init.d/rsyslog restart

debug "Wait for 30s and check on Whapps & ecallmgr status"
/bin/sleep 30
/etc/init.d/kz-whistle_apps status

debug "Attach ecallmgr to freeswitch"
/opt/kazoo/utils/sup/sup -n ecallmgr ecallmgr_maintenance add_fs_node freeswitch@$fqhn

#Kazoo-UI configurations
debug "Edit config.js with actual IP address"
/bin/sed -i s/'api.2600hz.com'/$ip_address/g /var/www/html/kazoo-ui/config/config.js

debug "Show change of IP"
/bin/cat /var/www/html/kazoo-ui/js/config.js

debug "Start Apache"
/etc/init.d/httpd start

#Create new account
debug "Create test account"
/usr/bin/sup crossbar_maintenance create_account $account_name $realm $user $pass


debug "Importing media files"
/opt/kazoo/utils/media_importer/media_importer /opt/kazoo/system_media/*.wav

debug "Congratulations, your server is now running an All-in-One Kazoo system!"

exit
