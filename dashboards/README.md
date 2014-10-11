# dashboards
=================

There are a couple of mechanisms you can deploy that will get your Kazoo data into dashboards/dashboarding systems.  This will explain some of those options.

## Method 1: Graphite / Grafana / InfluxDB / sup / Crossbar / etc ( i.e. Graphed statistics )

This section explains how to take time-series data you can gather from your servers (i.e. Concurrent Calls, Active Registrations, etc), stick them in a time-series 
database like InfluxDB, and get them up on a display using something like Grafana to display them beautifully.

You'll need a few things to get this to work.  I may glaze over some bits, please feel free to comment on anything that needs more explanation.

I've put things in order of necessity, if you're comortable and understand what each component is for, feel free to skip parts.

### InfluxDB (data store, time series database)

Install InfluxDB 

Full instructions here: http://influxdb.com/docs/v0.8/introduction/installation.html

NOTE: InfluxDB will need to listen on (by default anyways) TCP Ports 2003, 8083, 8086 

Quick instructions:

```
# for 64-bit systems
wget http://s3.amazonaws.com/influxdb/influxdb-latest-1.x86_64.rpm
sudo rpm -ivh influxdb-latest-1.x86_64.rpm

# for 32-bit systems
wget http://s3.amazonaws.com/influxdb/influxdb-latest-1.i686.rpm
sudo rpm -ivh influxdb-latest-1.i686.rpm
```

Edit your configs

It should install into /opt/influxdb, so edit /opt/influxdb/shared/config.toml and fill it like so:

```
bind-address = "0.0.0.0"
reporting-disabled = false
[logging]
level  = "info"
file   = "/opt/influxdb/shared/log.txt"         # stdout to log to standard out
[admin]
port   = 8083              # binding is disabled if the port isn't set
assets = "/opt/influxdb/current/admin"
[api]
port     = 8086    # binding is disabled if the port isn't set
read-timeout = "5s"
[input_plugins]
# Configure the graphite api
[input_plugins.graphite]
enabled = true
port = 2003
database = "grafana-stats"  # store graphite data in this database
[input_plugins.udp]
enabled = false
[raft]
port = 8090
dir  = "/opt/influxdb/shared/data/raft"
[storage]
dir = "/opt/influxdb/shared/data/db"
write-buffer-size = 10000
default-engine = "rocksdb"
max-open-shards = 0
point-batch-size = 100
write-batch-size = 5000000
retention-sweep-period = "10m"
[storage.engines.leveldb]
max-open-files = 1000
lru-cache-size = "200m"
[storage.engines.rocksdb]
max-open-files = 1000
lru-cache-size = "200m"
[storage.engines.hyperleveldb]
max-open-files = 1000
lru-cache-size = "200m"
[storage.engines.lmdb]
map-size = "100g"
[cluster]
protobuf_port = 8099
protobuf_timeout = "2s" # the write timeout on the protobuf conn any duration parseable by time.ParseDuration
protobuf_heartbeat = "200ms" # the heartbeat interval between the servers. must be parseable by time.ParseDuration
protobuf_min_backoff = "1s" # the minimum backoff after a failed heartbeat attempt
protobuf_max_backoff = "10s" # the maxmimum backoff after a failed heartbeat attempt
write-buffer-size = 1000
max-response-buffer-size = 100
concurrent-shard-query-limit = 10
[wal]
dir   = "/opt/influxdb/shared/data/wal"
flush-after = 1000 # the number of writes after which wal will be flushed, 0 for flushing on every write
bookmark-after = 1000 # the number of writes after which a bookmark will be created
index-after = 1000
requests-per-logfile = 10000
```

Then fire it up

```
/etc/init.d/influxdb start
```

Follow instructions for creating your first database here: http://influxdb.com/docs/v0.8/introduction/getting_started.html

I'll make the presumption you're going to call your database grafana-stats


### Graphite-API (API I/O layer to feed metrics into DB, accept metrics from diamond, netcat, other sources, and also to be queried by Grafana)

We're using just the thinnest version of Graphite, a small python script called Graphite-API (URL).  It speaks the expected graphite "language" so things like grafana and diamond can speak to it, but we've replaced its data store with InfluxDB. 

Get Graphite-API from here: https://github.com/brutasse/graphite-api
Graphite-API docs are here: http://graphite-api.readthedocs.org/en/latest/

Install Graphite-API

Personally, I installed using pip (you must yum install python-pip first - note: You must have EPEL repos installed)

```
pip install graphite-api
```

Configure Graphite-API

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

Start Graphite-API

It will be listening on tcp port 8000, as you can see in the command line below. If you want to change that, go ahead.  Make sure its accessible though.

```
/usr/bin/python /usr/bin/gunicorn -b 0.0.0.0:8000 -w 2 --log-level debug graphite_api.app:app
```
### Netcat

Install netcat (yum install nc)

Test a basic metric, the format is:  METRICNAME METRICVALUE POSIXDATESTAMP

```
TIMESTAMP=`date +%s`
METRIC=metric.test.thing
VALUE=10

echo $METRIC $VALUE $TIMESTAMP | nc HOST_OR_IP_OF_INFLUXDB_SERVER 2003
```

Now, go browse the InfluxDB web admin page and see if you can do "select * from metric.test.thing;" It should return one value of 10.


### Grafana


### Diamond

## Method 2: Log data from rsyslog -> logstash -> elasticsarch -> kibana

Other kinds of data you need to write queries to parse your log data (be it kamailio logs, 2600hz-platform logs, freeswitch logs, etc) and display the compiled data in a meaningful way (i.e. most popular account receiving REGISTER packets, most popular destination city for outgoing calls, etc.)

