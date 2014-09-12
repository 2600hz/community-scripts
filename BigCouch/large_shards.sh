#!/bin/bash

for DB in `find /srv/db/shards/ -size +$1 -exec ls -l {} \; | awk '{ print $5, $9 }' | sort -rn | cut -d' ' -f2 | sed -r 's|^.{33}||;s|\..*$||' | uniq`; do echo sup couch_compactor_fsm compact_db \"bigcouch@$(hostname -f)\" \"$DB\"; done
