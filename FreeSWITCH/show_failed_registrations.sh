#!/bin/sh

# usage: ./show_failed_registrations.sh /var/log/freeswitch/error.log
# can also use it on the rotated logs, eg error.log.1, error.log.2.gz, etc

zgrep "sofia_reg.c:2394 Can't find user" $1 | awk -F " " '{ printf "%s\n", $8}' | sort | uniq -c | sort -n
