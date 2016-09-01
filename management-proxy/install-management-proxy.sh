#!/usr/bin/env bash

##
##            install-management-proxy.sh
##     Installs Nginx, index page and proxy configs
##


## Detects which OS and if it is Linux then it will detect which Linux Distribution.
OS=`uname -s`
if [ "${OS}" = "SunOS" ] ; then
    OS=Solaris
    DIST=`uname -v`
    NGINX_DIR="/etc/opt/csw/nginx"
    COPY_CMD="cp -p"
elif [ "${OS}" = "Linux" ] ; then
    NGINX_DIR="/etc/nginx"
    if [ -f /etc/redhat-release ] ; then
        DIST='RedHat'
    elif [ -f /etc/debian_version ] ; then
        DIST="Debian"
    fi
    COPY_CMD="cp -p -v"
else
    NGINX_DIR="/etc/nginx"
    COPY_CMD="cp -p"
fi
#echo "Detected   OS = $OS   DIST = $DIST"; # for debug


## Install Nginx if NGINX_DIR missing
if [ ! -d $NGINX_DIR ] ; then
    EXIT_CODE=999
    if [ "${OS}" = "Solaris" ] ; then
        if [ -f /opt/csw/bin/pkgutil ] ; then
            echo -e "\t\tInstalling Nginx web server"
            /opt/csw/bin/pkgutil --install --yes CSWnginx
            EXIT_CODE=$?
        else
            echo "Error: Executable pkgutil not found."
            echo "Please install Nginx manually and rerun this script."
            exit 5
        fi
    elif [ "${DIST}" = "RedHat" ] ; then
        echo -e "\t\tInstalling Nginx web server"
        /usr/bin/yum install nginx
        EXIT_CODE=$?
    elif [ "${DIST}" = "Debian" ] ; then
        echo -e "\t\tInstalling Nginx web server"
        /usr/bin/apt-get install nginx
        EXIT_CODE=$?
    else
        echo "Error: Your OS or distro is not supported."
    fi

    ## Exit if installation failed
    if [ ! $EXIT_CODE -eq 0 ] ; then
        echo "Error: Installation failed."
        echo "Please install Nginx manually and rerun this script."
        exit 5
    fi

    ## remove default server
    echo -e "\n\t\tRemoving Nginx default server"
    if [ "${OS}" = "Solaris" ] ; then
        LINE=`grep -in "http {" /etc/opt/csw/nginx/nginx.conf | cut -d: -f1`
        if [ "$LINE" -gt "5" ] ; then
            LINE=`echo "$LINE + 1" | bc`
            cp -p /etc/opt/csw/nginx/nginx.conf /etc/opt/csw/nginx/nginx.conf.orig
            head -n $LINE /etc/opt/csw/nginx/nginx.conf.orig > /etc/opt/csw/nginx/nginx.conf
            echo "    include /etc/opt/csw/nginx/conf.d/*.conf;" >> /etc/opt/csw/nginx/nginx.conf
            echo "}" >> /etc/opt/csw/nginx/nginx.conf
        fi
        # make symlink since SSL Certificate path is under "/etc/nginx"
        ln -s /etc/opt/csw/nginx /etc/nginx
    fi
fi


## Recheck NGINX_DIR since install may have changed the situation
if [ -d $NGINX_DIR ] ; then
    echo -e "\n\tFound Nginx configuration directory: $NGINX_DIR"
else
    echo "Error: Couldn't find Nginx configuration directory: $NGINX_DIR"
    echo "Error: Installation failed."
    echo "Please install Nginx manually and rerun this script."
    exit 6
fi


## Copy files into place
echo -e "\n\t\tCopying files"
mkdir -p $NGINX_DIR/conf.d
${COPY_CMD} management-proxy.conf $NGINX_DIR/conf.d/
mkdir -p /var/www/html
${COPY_CMD} index-management.html /var/www/html/


## Generate selfsigned certificate and private key
echo -e "\n\t\tGenerating certificate"
echo -e "\n\tPress Enter to select default values.\n"
openssl req -x509 -nodes -days 3650 -newkey rsa:4096 \
  -config ./openssl-management-proxy.cnf \
  -keyout $NGINX_DIR/conf.d/management-proxy.key \
  -out $NGINX_DIR/conf.d/management-proxy.crt


## Enable rabbitmq_management plugin
echo -e "\n\t\tEnabling rabbitmq_management plugin"
rabbitmq-plugins enable rabbitmq_management


## Enable/Start Nginx
echo -e "\n\t\tEnabling Nginx web server"
if [ "${OS}" = "Solaris" ] ; then
    svcadm disable svc:/network/cswnginx:default > /dev/null 2>&1
    sleep 1
    pkill -9 /opt/csw/sbin/nginx > /dev/null 2>&1
    sleep 1
    svcadm -v enable svc:/network/cswnginx:default
    sleep 3
    svcs svc:/network/cswnginx:default
else
    # If systemd lives here, then we have a systemd unit file
    if pidof systemd > /dev/null 2>&1; then
        systemctl enable nginx
    else
        echo "Sorry: This script only knows how to enable Nginx on Solaris/SMF and Linux/systemd."
        echo "Please enable Nginx manually."
    fi
    service nginx stop > /dev/null 2>&1
    service nginx start
fi
