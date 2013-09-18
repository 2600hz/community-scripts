#!/bin/sh

# show User-Agent breakdown
# usage: ./user_agents.sh /var/log/freeswitch/debug.log

grep "User-Agent:" $1 | sort | uniq -c | sort -n
