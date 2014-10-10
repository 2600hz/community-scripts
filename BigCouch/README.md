# Bigcouch Utils
=================

## large_shards.sh \<size\>
Finds DB shards exceeding `size` and provides the SUP command to compact

```bash
[root@db002-dev BigCouch]# ./large_shards.sh 10M
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "services"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "ratedeck"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "signups"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "offnet"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "webhooks"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "accounts"
sup couch_compactor_fsm compact_db "bigcouch@db002-dev.2600hz.com" "ratedeck"
```

## shard_ratio.sh \<size\>
Returns the ratio of disk to data size for all shards exceeding `size`.

_Note:_ Requires bc `yum install -y bc`

```bash
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
