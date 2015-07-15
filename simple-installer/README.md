# community-scripts
=================

Public Scripts from the 2600hz Community

## simple-installer

* Simple-installer for installing the stable, staging or latest version of Kazoo on centos 6.5.
* This is the same script packaged into the ISO.


### How To install: 
* get all the files from the repo and place them in /opt/kazoo_install 
* run 
```bash 
chmod +x /opt/kazoo_install/setup*
chmod +x /opt/kazoo_install/install*  
``` 
* to start the install run 
```bash
./install_kazoo
```

_NOTE_: This is a network based installer, so you should verify your internet connectivity is working prior to installing. 


### Scripts: 
The installer breaks functionality up into seperate scripts. The installer script will install the packages selected by the user. Then setup scripts are executed based on package selection to configure each of the services that were installed. In all in one mode, the server will setup all the packages using the all in one defaults. 

#### Install Scripts:
**install_kazoo**    - Installs the RPM packages for kazoo based on user package selection and then calls setup_packages.

**setup_packages**   - Dispatches to setup scripts for every package specified in command line args. 

**setup_freeswitch** - FreeSWITCH setup script. 

**setup_kazoo**      - Kazoo setup script. 

**setup_rabbitmq**   - RabbitMQ setup script. 

**setup_monster-ui** - Monster-UI setup script.

**setup_kamailio**   - Kamaiio setup script. 

**setup_bigcouch**   - BigCouch setup script. 

**setup_haproxy**    - HaProxy setup script. 

**setup_common**     - a bash function "library" to provide all the commonly used functions in the setup scripts. 

#### Other scripts: 
**onboot_kazoo**     - Script used to check if the IP_ADDRESS value in /etc/kazoo/kazoo_boot.conf matches the systems IP address. 

**get_ip_address**   - Script used for selecting IP address on boot if onboot_kazoo is unable to automatically determine which IP address to set if system IP was changed. This script is called on login via the root users .bashrc so the user is prompted to change the kazoo IP addresses. 

**kazoo_motd**       - Script that generates a dynamic motd which includes some nice details about the kazoo system. 











