blackhole-client
================

**blackhole-client** is a node.js console application for connecting to kazoo's websockets application blackhole. 

**blackhole-client** utilizes a node.js kazoo crossbar library ([https://github.com/macpie/crossbar-nodejs](https://github.com/macpie/crossbar-nodejs)) for gaining authentication to Kazoo.

#### Application Setup Instructions
**blackhole-client** requires node.js.  Follow [these instructions](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager) to install node on your target machine.

You will need git installed in order to clone the blackhole-client repo and to install the package dependencies for **blackhole-client** (crossbar lib).

###### Follow these instructions to get blackhole-client setup

* mkdir /usr/local/src/blackhole-client
* git clone https://github.com/tickbw/blackhole-client.git /usr/local/src/blackhole-client
* cd /usr/local/src/blackhole-client
* npm install
* cp config.js.sample config.js
* vim config.js (Make Edits to the file that reflect your kazoo server details and credentials)
* node app

###### Example config.js file

    var config = {};  
    config.blackhole = {};  
    config.crossbar = {};  
    
    config.blackhole.host = 'http://192.168.56.111';  
    config.blackhole.port = 5555;  
    config.crossbar.host = 'http://192.168.56.111';  
    config.crossbar.port = 8000;  
    config.crossbar.username = "bwann";  
    config.crossbar.password = "12341234";  
    config.crossbar.account_name = "account name";  
    module.exports = config;
