#!/bin/sh

## Replace the find command with whatever will print the full path of the .couch file. The output will be the curl command necessary to compact that shard.

find /srv/db/shards/ -name "*201501*" | awk -F '/' 'BEGIN {OFS="%2F"} {print "curl -X POST -H \"Content-type:application/json\" localhost:5986/" $4, $5, $6, $7, $8, $9 "/_compact"}' | sed -e 's/\.couch//g' > /tmp/compact.me
