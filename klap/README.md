# KLAP
================
Kazoo Log Analysis Program

## stats.sh \<path\>
Runs all the analyzers on kazoo log(s). 

_Note:_ Each analyzer can be run individually.

```bash
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
