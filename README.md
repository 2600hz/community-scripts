# community-scripts
=================

Public Scripts from the 2600hz Community

## Bigcouch

### large_shards.sh <size>
Finds DB shards exceeding `size` and provides the SUP command to compact

```
[root@db002-dev BigCouch]# ./large_shards.sh 10M
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "services"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "ratedeck"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "signups"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "offnet"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "webhooks"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "accounts"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "ratedeck"
```

### shard_ratio.sh <size>
Returns the ratio of disk to data size for all shards exceeding `size`.

_Note:_ Requires bc - `yum install -y bc`

```
[root@db002-dev BigCouch]# ./shard_ratio.sh 10M
13154786 signups
8253836 ratedeck
8253836 ratedeck
1538251 offnet
1453137 webhooks
265489 acdc
223425 accounts
223425 accounts
144278 services
144278 services
```

## FreeSWITCH

### user_agents.sh <path>
Show the number of times unique user-agents occur in a FreeSWITCH debug log.
```
[root@fs001-dev FreeSWITCH]# ./user_agents.sh /var/log/freeswitch/debug.log.1 
      4    User-Agent: PolycomVVX-VVX_500-UA/5.0.0.6874
     16    User-Agent: 2600hz
```

### sipify.sh <path>
The FreeSWITCH debug log does not include the call-id on the lines with the SIP headers.  This script will add them so they are included in when grep'd or otherwise searched.  

_Note:_ This can consume large amounts of memory, and impact the running system.  Use with caution.

```
[root@fs001-dev FreeSWITCH]# ./sipify.sh /var/log/freeswitch/debug.log.1 | grep 46a1fe62-20f73e9-1c6e7dbc@10.26.0.199                                 
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199 recv 1141 bytes from udp/[10.26.0.81]:5060 at 19:34:41.657063:
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    ------------------------------------------------------------------------
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    INVITE sip:*97@kanderson.dev.2600hz.com;user=phone SIP/2.0
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Record-Route: <sip:10.26.0.81;lr=on;ftag=C4F24CBE-866C8875>
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Via: SIP/2.0/UDP 10.26.0.81;branch=z9hG4bKae67.eae1c251.0
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Via: SIP/2.0/UDP 10.26.0.199:5060;rport=5060;branch=z9hG4bKb8aa0385D3717F7
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    From: "user_ngmdk8" <sip:user_ngmdk8@kanderson.dev.2600hz.com>;tag=C4F24CBE-866C8875
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    To: <sip:*97@kanderson.dev.2600hz.com;user=phone>
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    CSeq: 1 INVITE
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Call-ID: 46a1fe62-20f73e9-1c6e7dbc@10.26.0.199
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Contact: <sip:user_ngmdk8@10.26.0.199:5060>
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, INFO, MESSAGE, SUBSCRIBE, NOTIFY, PRACK, UPDATE, REFER
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    User-Agent: PolycomVVX-VVX_500-UA/5.0.0.6874
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Accept-Language: en
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Supported: 100rel,replaces
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Allow-Events: conference,talk,hold
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Content-Type: application/sdp
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Content-Length: 332
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    X-AUTH-IP: 10.26.0.199
```

## RadiusCDR
Script to listen for incoming CDRs from the RabbitMQ server, store the CDR in a sqlite database, and send radius account start and stop packets for billing

## kazoo-puppet
Puppet scripts for deploying Kazoo (requires an update, only valid for single server currently)

_Note:_ These have not been maintained and likely need updating to properly deploy kazoo with the recent changes to configuration.

## simple-installer
Kazoo install tool that can be used to set up an all-in-one server or assist with cluster deployment.

## CloneTools
Erlang tool to copy all databases from one Bigcouch cluster to another, with options for CDRs and voicemails

### Usage
Edit `src/clone_tools.hrl`:
-define(TARGET, "http://127.0.0.1:15984/"). %%URL of HAProxy for the new database cluster
-define(SOURCE, "http://127.0.0.1:15984/"). %%A single bigcouch node in the old (current) database cluster.

Optionally change MAX_CR_AGE and MAX_VM_AGE. These control the maximum age (in days) for CDRs and voicemail messages, respectively.  Setting the value to 0 will copy all documents of the indicated type. Setting to any positive integer will copy all documents of that type up to that number of days prior to the execution of the clone tool.  You can also set the value to 'none' to skip all documents of the respective type.

In the clone_tools directory run:
```
make clean;make;./clone.sh
```

This will start a clone, it will take a very long time (possibly days) and to ensure it is not disrupted we recommend running it in screen.

### Few Notes
* This is a one way copy, changes made during the copy will not be cloned on subsequent runs (expect newly created documents and voicemails)
* For the lowest possibility of lost changes the new cluster should be put in service as soon as the clone is complete
* No CDRs are cloned, this could be done later while the new cluster is in service, however we do not currently have a tool available to do so
* It IS safe to stop and restart the script, however remember the longer a cloned database is unused the greater a chance changes to the original will be lost

### How it works
* It reads all databases on the SOURCE system and starts cloning the list
* If the current db being cloned IS NOT an account/XX/XX/XXXXX db
** It will get a list of all document IDs in both the SOURCE and TARGET.  It will then clone any IDs that only exist on the SOURCE
* If the current db being cloned IS an account/XX/XX/XXXX db then it will (in detail):
** create the database on the TARGET
** add a view to the SOURCE db
** copy all views to the TARGET (including the newly added "clone" view)
** query the clone view for a list of all document IDs that are NOT cdrs, acdc_stats, credit, debit, or vmbox on both SOURCE and TARGET.  It will then clone any IDs that only exist on the SOURCE
** query the clone view for a list of all documents with attachments on both SOURCE and TARGET.  The results of the view also includes the total size of all attachements on each document.  If this differs then the attachments are copied from the SOURCE to the TARGET
** find all vmboxes in SOURCE and OVERWRITE them on the TARGET.  This ensures if a voicemail was left while the clone ran, you can re-run it just prior to the cut over to ensure it is present.  However, this also means if you run this script after a voicemail is left via the TARGET it will be lost.  The proper use case is to run the script, then re-run it immediately BEFORE switching Kazoo to use the TARGET, but NEVER after :)
** query the current available credit in the db and create one transaction on the TARGET to "roll-up" (represent) all the transaction history on the SOURCE.  Note, this is a single document representing the available credit at the time of the clone so the history is lost but since we dont expose it currently this is not an issue unless the client has written a tool to use it themselfs.

## sipp

This is a collection of SIPp templates and scripts.

## klap
Kazoo Log Analysis Program

### stats.sh <path>
Runs all the analyzers on kazoo log(s). 

_Note:_ Each analyzer can be run individually.

```
[root@apps001-dev klap]# ./stats.sh /var/log/2600hz/kazoo.log 
/var/log/2600hz/kazoo.log
  Start                    : Sep 12 00:26:36
  End                      : Sep 12 18:02:24
  Duration                 : 63348s

 ACCOUNTS
  Unique Account DBs       : 154
  Most Frequent Account DBs
    142    account%2F4a%2F53%2Fc217aaacb14aa94bc41230ba433f
    1      account%2Fe0%2Fd0%2Fd9935226594eeed62e9d1a214762
    1      account%2Fa0%2Ff3%2Fb6f2c5c0c95240993acd1bd6e762
    1      account%2F9c%2F88%2F222a68297479928cb8f2a4624cab
    1      account%2F91%2F42%2F021acb03d27887e47ff3b858c826
    1      account%2F87%2F48%2F22c99b19d7d2cc343c108a71ccba
    1      account%2F80%2F7e%2F95c4b4ade47cb4a3a82dd1a3aacd
    1      account%2F7c%2F1f%2Fdcc8f457eacae29063d0c06f3305
    1      account%2F4f%2F02%2Fec8424f715cc83df2c1cc3d82c0e
    1      account%2F28%2Fab%2Fa7777b38502b99c39489a73d1589

 AMQP-WORKER
  Requests                 : 874
  Unexpected Message       : 0
  Timeouts                 : 0
  Failed Publishes         : 0
  Lookup Histogram

 AMQP
  Published                : 7355
  Channels                 : 0
  Queues                   : 0
  Consumers                : 0
  Bindings                 : 0
  Dropped Methods          : 0
  Short Lived Channels     : 0
  Connections              : 0
  Most Frequent Routing Keys
    134    registration.query.test%2E2600hz%2Ecom.*
    134    registration.query.subaccount1%2Edev%2E2600hz%2Ecom.*
    134    registration.query.eb0827%2Esip%2E2600hz%2Ecom.*
    134    registration.query.d1ee53%2Esip%2E2600hz%2Ecom.*
    134    registration.query.62b63f%2Esip%2E2600hz%2Ecom.*
    134    registration.query.3191cb%2Esip%2E2600hz%2Ecom.*
    70     call.status_req.channels
    3      kamailio@apps001-dev.2600hz.com-<12950-23377>-targeted-48
    2      kamailio@apps001-dev.2600hz.com-<12950-23377>-targeted-98
    2      kamailio@apps001-dev.2600hz.com-<12950-23377>-targeted-97

 CALL-IDS
  Unique Call-IDs          : 162

 COUCH-MGR
  Unformatted Errors

 HANGUPS
  Abnormal Hangup Causes

 JONNY5
  Allowed Calls            : 0
  Disallowed call          : 0
  Emergency Exceptions     : 0
  Toll-free Exceptions     : 0

 REGISTRAR
  Found Credentials        : 141
  Missing Credentials      : 0
  Disabled Credentials     : 0
  Credential Lookup Errors : 0
  Found Auth-by-IP         : 0
  Missing Auth-by-IP       : 0
  Auth-by-IP Lookup Errors : 0
  Highest failures

 SYSTEM
  Unique PIDs              : 3967
  Most Frequent PIDs
    73179  <0.803.0>
    7772   <0.1019.0>
    6340   <0.806.0>
    397    <0.28116.27>
    120    <0.6276.27>
    70     <0.1736.0>
    44     <0.6288.27>
    27     <0.426.0>
    27     <0.425.0>
    27     <0.424.0>
  Most Frequent Lines of Code
    17974  couch_compactor_fsm:1632
    17520  couch_compactor_fsm:672
    17472  couch_compactor_fsm:679
    16450  couch_compactor_fsm:1662
    7355   wh_amqp_channel:153
    6336   hangups_monitoring:171
    1742   notify_account_crawler:307
    1742   notify_account_crawler:183
    1608   wh_topup:29
    1118   couch_compactor_fsm:1659
  Most Active Times
    494    Sep 12 16:29
    210    Sep 12 14:40
    146    Sep 12 15:06
    141    Sep 12 13:55
    141    Sep 12 07:37
    140    Sep 12 17:36
    140    Sep 12 14:58
    140    Sep 12 09:58
    140    Sep 12 07:45
    138    Sep 12 16:55
  OS Processes
    2600hz[3960]
```
