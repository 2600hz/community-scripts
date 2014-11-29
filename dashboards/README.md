# dashboards

There are a couple of mechanisms you can deploy that will get your Kazoo data into dashboards/dashboarding systems.  This will explain some of those options.

## Method 1: Graphite / Grafana / InfluxDB / sup / Crossbar / etc ( i.e. Graphed statistics )

This section explains how to take time-series data you can gather from your servers (i.e. Concurrent Calls, Active Registrations, etc), stick them in a time-series 
database like InfluxDB, and get them up on a display using something like Grafana to display them beautifully.

You'll need a few things to get this to work.  I may glaze over some bits, please feel free to comment on anything that needs more explanation.

I've put things in order of necessity, if you're comortable and understand what each component is for, feel free to skip parts.

### Step 1: InfluxDB 
Purpose: Influx is the data store, a fast time series database

#### Install InfluxDB 

Full instructions here: http://influxdb.com/docs/v0.8/introduction/installation.html
As of this writing, the current version is 0.8.3

NOTE: InfluxDB will need to listen on (by default, anyways) TCP Ports 2003 (a graphite input handler), 8083 (admin gui), 8086 (api url) 

Quick instructions:

```
# for 64-bit systems
wget http://s3.amazonaws.com/influxdb/influxdb-latest-1.x86_64.rpm
sudo rpm -ivh influxdb-latest-1.x86_64.rpm

# for 32-bit systems
wget http://s3.amazonaws.com/influxdb/influxdb-latest-1.i686.rpm
sudo rpm -ivh influxdb-latest-1.i686.rpm
```

#### Configure InfluxDB

It should install into /opt/influxdb, so edit /opt/influxdb/shared/config.toml.  All you really have to do is find the graphite input handler plugin and enable it, set the port, and specify the database to write to, like so:

```
  [input_plugins.graphite]
  enabled = true
  port = 2003
  database = "grafana-stats"  # store graphite data in this database
```

The database hasnt been created yet, but I'll make the presumption you're going to call your database __grafana-stats__


#### Start InfluxDB

```
/etc/init.d/influxdb start
```

Either follow the official instructions for creating your first database here: http://influxdb.com/docs/v0.8/introduction/getting_started.html

-or-

My Quick Setup Instructions:  Hit http://ip_or_hostname_of_influxdb_server:8083

Login with username root password root - then click cluster admins, and change your password right away.

Click disconnect in the top right, then log back in with your new username and password.

Click databases, fill in the database name __grafana-stats__ and then click Create Database.  You can leave the shard defaults as is.

Click on your newly created database name, and add a user that will be used to put data in this database only.  Write it down cause you'll need it in step 2.  You dont need to make it an admin user.

Now repeat the database + user creation process once more, this time for a database we'll call __grafana-dashboards__ where grafana will store its saved dashboards.

InfluxDB is all done!


### Step 2: Graphite-API 
Purpose: Grafana (or other tools) that are written to query Graphite can instead query a lightweight Graphite-API; and Graphite-API knows how to read natively from InfluxDB

We're using just the thinnest version of Graphite, a small python script called Graphite-API (https://github.com/brutasse/graphite-api).  It speaks the expected graphite "language" so things like grafana can speak to it, but we've replaced its data store with InfluxDB. 

Graphite-API docs are here: http://graphite-api.readthedocs.org/en/latest/

#### Install Graphite-API

First, lets install the [EPEL](https://fedoraproject.org/wiki/EPEL) repo for Centos 6. Click the link if you need the repo for another version.

```
rpm -ivh http://fedora-epel.mirror.iweb.com/6/i386/epel-release-6-8.noarch.rpm
```

Then lets install pip and the rest of the packages and tools you'll need:

```
yum install python-pip gcc libffi-devel python-devel cairo
pip install influxdb
pip install graphite-influxdb
pip install Flask-Cache
```

Currently, we need to download a patched version of graphite-api that supports caches, statsd instrumentation, and graphite-style templates.

```
cd /tmp
wget -O graphite-api-patched.tar.gz https://github.com/Dieterbe/graphite-api/tarball/support-templates2
tar zxvf graphite-api-patched.tar.gz
cd Dieterbe-graphite-api-739165f/
python setup.py install
```

You'll also need some supporting packages in order to run graphite-api. You can serve it from apache, nginx, gunicorn, whatever you want.  I've used straight-up gunicorn for simplicity here, so make sure gunicorn is avaiable on your system. For more detailed instructions on hosting behind nginx, apache, etc, see the graphite-api docs here: http://graphite-api.readthedocs.org/en/latest/deployment.html

So let's get gunicorn on CentOS:

```
yum install python-gunicorn
```

#### Configure Graphite-API

Create the file /etc/graphite-api.yaml and fill it like so:

```
search_index: /data/graphite/storage/index
finders:
  - graphite_influxdb.InfluxdbFinder
influxdb:
   host: HOSTNAME_OR_IP_OF_SERVER_WITH_INFLUXDB_ON_IT
   port: 8086
   user: INFLUXDB_USER_YOU_CREATED
   pass: INFLUXDB_PASSWORD_YOU_CREATED
   db:   grafana-stats
   cheat_times: true
cache:
    # memcache seems to have issues with storing the series. the data is too big and doesn't store it, and doesn't allow bumping size in config that much
    #CACHE_TYPE: 'memcached'
    #CACHE_KEY_PREFIX: 'graphite_api'
    CACHE_TYPE: 'filesystem'
    CACHE_DIR: '/tmp/graphite-api-cache'
logging:
  version: 1
  disable_existing_loggers: true
  handlers:
    file:
      class: logging.FileHandler
      filename: /var/log/graphite-api.log
    stdout:
      class: logging.StreamHandler
  loggers:
    graphite_api:
      handlers:
        - stdout
      propagate: true
      level: INFO
    graphite_influxdb:
      handlers:
        - stdout
      propagate: true
      level: INFO
    root:
      handlers:
        - stdout
      propagate: true
      level: INFO
```

The search_index file path needs to exist. You can place it anywhere you like, but to keep with the path in the example, you'll need to create the directory and make sure whichever user you're running gunicorn as can write to it:

```
mkdir -p /data/graphite/storage/
```

#### Start Graphite-API

It will be listening on tcp port 8000, as you can see in the command line below. If you want to change that, go ahead.  Make sure its accessible though.  Note, my command line below wil run it in the background.  Remove the & at the end to run it in the foreground if you want to test for now.

```
/usr/bin/python /usr/bin/gunicorn -b 0.0.0.0:8000 -w 2 --log-level debug graphite_api.app:app &
```

### Step 3: Netcat 
The most lightweight, simple, quick way of taking a value and putting it in the database (note: this is not encrypted at all)

#### Install netcat 

```
yum install nc
```

#### Test a basic metric

The graphite metric format is:  METRICNAME METRICVALUE POSIXDATESTAMP

Here's a small script you can use to test it:

~~~
#!/bin/sh
TIMESTAMP=`date +%s`
METRIC=metric.test.thing
VALUE=10

echo $METRIC $VALUE $TIMESTAMP | nc HOST_OR_IP_OF_INFLUXDB_SERVER 2003
~~~

Now, go browse the InfluxDB web admin page (http://IP_OF_YOUR_INFLUXDB:8083), click on "Explore data" next to your database __grafana-stats__  and see if you can do "select * from metric.test.thing;" It should return one value of 10.


### Step 4: Grafana
Purpose: To actually draw the awesome dashboards! It wants to speak to a graphite server, which graphite-api is handling for us, which will then in turn query InfluxDB.

#### Install Grafana

We're going to install Grafana via grafana-authentication-proxy.  It's a small wrapper that adds proper authentication and per-user-dashboards to grafana. Its not the usual way you'd install it, but trust me, you'll want it. So, install grafana-authentication-proxy.  I put mine in /usr/local:

```
cd /usr/local
yum install -y git
git clone https://github.com/voxter/grafana-authentication-proxy.git
cd grafana-authentication-proxy/
git submodule init
git submodule update
yum install -y npm nodejs-grunt-cli
npm install
```

Update grafana to the latest version (and base it off the proper repo):

```
cd grafana && git checkout master && git pull
npm install
grunt
```

#### Configure Grafana / Grafana-authentication-proxy

Edit /usr/local/grafana-authentication-proxy/config.js

Depending on wether or not you're using elasticsarch (in this case, it would be for storing user dashboards) you can specify your ES server information here too, and it will proxy front end user auth for ES queries as well. Otherwise you can leave it alone, and we'll store our dashboards in InfluxDB

Pay attention to the following lines in the config file and update them accordingly:

```
    "graphiteUrl": "http://ip_or_host_of_your_graphite_api_server:8000", // This address must be publicly reachable

    "cookie_secret": "cookie_rhymes_with_c",
    
    "enable_basic_auth": true,
    
    "basic_auth_file": "",
        "basic_auth_users": [
            {"user": "username1", "password": "passwd1"},
            {"user": "username2", "password": "passwd2"},
        ],
```

You can choose to enable Google OAuth, or basic auth, or whatever you want - but this example we've only shown how to enable Basic Auth.

Once your config is done, start the node application with:

```
cd /usr/local/grafana-authentication-proxy
node app.js &

Server starting...
Info: HTTP Basic Authentication applied
Warning: No Google OAuth2 presented
Warning: No CAS authentication presented
Server listening on 9202
```

It will (by default) use InfluxDB to store saved dashboards.  You can also use elasticsearch if you've got that set up already, but since we didnt cover it in this tutorial, I'll skip that.


#### Deploy Grafana 

We're going to use nginx to serve grafana up over HTTP.

So first, make sure you've got nginx installed:

```
yum install -y nginx
```

Then take the grafana.conf I've included in this repo in the nginx/conf.d file, and place it in your own /etc/nginx/conf.d directory.

Make the necessary changes to the hostnames in the config file (if necessary), or simply remove /etc/nginx/conf.d/default.conf so the grafana.conf becomes the default host (if you dont have any other virtual hosts running on this server) and restart nginx.

#### Test it out!

Hit http://your_grafana_server in a web browser, it should ask you for one of the usernames you put in the grafana-authentication-proxy config.js file.  Once you log in, you should see the grafana page, and if everything is working properly, there should be a graph at the bottom of the page with simulated data on it!  Click the title of the graph, then click edit.

Click the green add query button, and then click the first 'select metric' box that has now appeared.  You should see "metric" - repeat, then you'll seee "test", once more and you'll see "thing" - this is the metric we added a moment ago when we were testing InfluxDB!

### Step 5: Collect some data!

OK, so we have a working datastore backend, a graphite Input handler, query API, and a frontend to make it all pretty.  Now, how do we get some kick ass system and kazoo data in there?

Let's start with a very simple bash script that we can cron.  I've included a script in this repo called system_stats.sh. Run it to make sure it works:

~~~
# ./system_stats.sh
kazoo.calls.inbound.net.voxter.dc1.prod.media01.total 7 1413233954
kazoo.calls.outbound.net.voxter.dc1.prod.media01.total 7 1413233954
kazoo.calls.inbound.net.voxter.dc1.prod.media02.total 7 1413233954
kazoo.calls.outbound.net.voxter.dc1.prod.media02.total 6 1413233954
~~~

This is the data that would have been sent to the graphite input handler, so you're set!  Stick that puppy in a crontab and come back in an hour.

~~~
/etc/crontab

*/10 * * * * root /usr/local/bin/graphite_log_total_calls > /dev/null 2>&1
~~~

Now go build yourself some dashboards using the data!




## Method 2: Log data from rsyslog -> logstash -> elasticsarch -> kibana

Other kinds of data you need to write queries to parse your log data (be it kamailio logs, 2600hz-platform logs, freeswitch logs, etc) and display the compiled data in a meaningful way (i.e. most popular account receiving REGISTER packets, most popular destination city for outgoing calls, etc.)

COMING SOON...
