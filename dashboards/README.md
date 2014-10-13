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

InfluxDB is all done!


### Step 2: Graphite-API 
Purpose: Grafana (or other tools) that are written to query Graphite can instead query a lightweight Graphite-API; and Graphite-API knows how to read natively from InfluxDB

We're using just the thinnest version of Graphite, a small python script called Graphite-API (https://github.com/brutasse/graphite-api).  It speaks the expected graphite "language" so things like grafana can speak to it, but we've replaced its data store with InfluxDB. 

Graphite-API docs are here: http://graphite-api.readthedocs.org/en/latest/

#### Install Graphite-API

Personally, I installed using pip (you must yum install python-pip first - note: You must have [EPEL](https://fedoraproject.org/wiki/EPEL) repos installed)

```
pip install graphite-api
```

You'll also need some supporting packages in order to run graphite-api. You can serve it from apache, nginx, gunicorn, whatever you want.  I've used gunicorn for simplicity here, so make sure gunicorn is avaiable on your system. 

On CentOS:

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

#### Start Graphite-API

It will be listening on tcp port 8000, as you can see in the command line below. If you want to change that, go ahead.  Make sure its accessible though.

```
/usr/bin/python /usr/bin/gunicorn -b 0.0.0.0:8000 -w 2 --log-level debug graphite_api.app:app
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
TIMESTAMP=`date +%s`
METRIC=metric.test.thing
VALUE=10

echo $METRIC $VALUE $TIMESTAMP | nc HOST_OR_IP_OF_INFLUXDB_SERVER 2003
~~~

Now, go browse the InfluxDB web admin page (http://IP_OF_YOUR_INFLUXDB:8083) and see if you can do "select * from metric.test.thing;" It should return one value of 10.


### Step 4: Grafana
Purpose: To actually draw the awesome dashboards! It wants to speak to a graphite server, which graphite-api is handling for us, which will then in turn query InfluxDB.

#### Install Grafana
Pre requisites: Elasticsearch or ???

Grab the latest grafana from http://grafana.org/download/
As of this writing, the latest is 1.8.1

Untar/unzip grafana into some place you intend to host it (like /usr/local/grafana-1.8.1)

#### Configure Grafana

Edit/create config.js file in the grafana directory, and use my config as an example if you like. (This file also in this repo)

~~~
///// @scratch /configuration/config.js/1
 // == Configuration
 // config.js is where you will find the core Grafana configuration. This file contains parameter that
 // must be set before Grafana is run for the first time.
 ///
define(['settings'],
function (Settings) {


  return new Settings({

    /* Data sources
    * ========================================================
    * Datasources are used to fetch metrics, annotations, and serve as dashboard storage
    *  - You can have multiple of the same type.
    *  - grafanaDB: true    marks it for use for dashboard storage
    *  - default: true      marks the datasource as the default metric source (if you have multiple)
    *  - basic authentication: use url syntax http://username:password@domain:port
    */

    // InfluxDB example setup (the InfluxDB databases specified need to exist)
    /*
    datasources: {
      // If you want to query influxdb directly instead of via graphite, you can do this, but you lose some functions that graphite provides.
    /*  influxdb: {
        type: 'influxdb',
        url: "http://MY_INFLUXDB_SERVER:8086/db/database_name",
        username: 'admin',
        password: 'admin',
      },
    */
      // You can use InfluxDB as the "grafanaDB" to store your dashboards instead of using ElasticSearch.  If you've already got an elasticsearch server, you can use it below.
    /*
      grafana: {
        type: 'influxdb',
        url: "http://MY_INFLUXDB_SERVER:8086/db/grafana",
        username: 'admin',
        password: 'admin',
        grafanaDB: true
      },
    */
    },
    */

    // Graphite & Elasticsearch example setup

    datasources: {
      graphite_api: {
        type: 'graphite',
        url: "http://MY_GRAPHITEAPI_SERVER",
        default: true,
      },
      elasticsearch: {
        type: 'elasticsearch',
        url: "http://MY_ELASTICSEARCH_SERVER:9202",
        index: 'grafana-dash',
        grafanaDB: true,
      },
      influxdb: {
        type: 'influxdb',
        url: "http://MY_INFLUXDB_SERVER:8086/db/grafana-stats",
        username: 'MY_GRAPHITE_USER',
        password: 'MY_GRAPHITE_PASSWORD',
      }
    },

    // OpenTSDB & Elasticsearch example setup
    /*
    datasources: {
      opentsdb: {
        type: 'opentsdb',
        url: "http://opentsdb.server:4242",
      }
    },
    */

    /* Global configuration options
    * ========================================================
    */

    // specify the limit for dashboard search results
    search: {
      max_results: 20
    },

    // default start dashboard
    default_route: '/dashboard/file/default.json',

    // set to false to disable unsaved changes warning
    unsaved_changes_warning: true,

    // set the default timespan for the playlist feature
    // Example: "1m", "1h"
    playlist_timespan: "1m",

    // If you want to specify password before saving, please specify it bellow
    // The purpose of this password is not security, but to stop some users from accidentally changing dashboards
    admin: {
      password: 'grafana_secret_password'
    },

    // Add your own custom pannels
    plugins: {
      panels: []
    }

  });
});
~~~

#### Deploy Grafana 

We're going to use nginx to serve grafana up over HTTP and although you dont __need__ it, I'm going to do you a favor and give you a working example of how to couple it with grafana-authentication-proxy (https://github.com/strima/grafana-authentication-proxy) so your grafana dashboards are password protected (not the default).

So first, make sure you've got nginx installed:

```
yum install nginx
```

Next, install grafana-authentication-proxy.  I put mine in /usr/local alongside grafana

```
# cd /usr/local
# git clone https://github.com/strima/grafana-authentication-proxy.git
# cd grafana-authentication-proxy/
# git submodule init
# git submodule update
# npm install
```

#### Add user auth/per-user dashboards to Grafana by using grafana-authentication-proxy 


### Step 5: Diamond

## Method 2: Log data from rsyslog -> logstash -> elasticsarch -> kibana

Other kinds of data you need to write queries to parse your log data (be it kamailio logs, 2600hz-platform logs, freeswitch logs, etc) and display the compiled data in a meaningful way (i.e. most popular account receiving REGISTER packets, most popular destination city for outgoing calls, etc.)

