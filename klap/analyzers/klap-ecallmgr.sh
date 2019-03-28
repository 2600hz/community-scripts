#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for F in $(listFiles $@); do
    FILE=$(realpath $F)
    CWD=$(pwd)

	createTmpFile "\|ecallmgr_"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

        printStat "Authorized channels" $(countMatches "channel is authorized$")

        printStat "New FS Connections" $(countMatches "successfully connected to freeswitch node")
        printStat "Lost FS Connections" $(countMatches "received node down notice")


        grep -Ea "successfully connected to freeswitch node|received node down notice" $TMP_FILE | sed -re "s/.+(\w\w:\w\w:\w\w).*(connected|down).*freeswitch@([^\w]+)/\1 \2 \3/p" | sort | uniq | printTable "FreeSWITCH connections"

        grep -a " updated registration " $TMP_FILE | grep -Po "@[\w\.]+" | sort | uniq -c | sort -nr | head | printTable "Registrations by realm"

        grep -a " channel is authorized by account " $TMP_FILE | cut -d' ' -f13,15 | sort | uniq -c | sort -nr | head | printTable "Authz type by AccountId"
        grep -a " channel is authorized by reseller " $TMP_FILE | cut -d' ' -f13,15 | sort | uniq -c | sort -nr | head | printTable "Authz type by ResellerId"

        grep -Pa "ecallmgr_fs_authn.+user directory " $TMP_FILE | grep -Po 'freeswitch@([^|]+)' | sort | uniq -c | sort -nr | printTable "Directory Reqs by FreeSWITCH"
        grep -Pa "ecallmgr_fs_route.+fetch request " $TMP_FILE | grep -Po 'freeswitch@([^|]+)' | sort | uniq -c | sort -nr | printTable "Dialplan Reqs by FreeSWITCH"

        # FreeSWITCH event streams
        grep -a "event stream for " $TMP_FILE | cut -d' ' -f3,14 | uniq -c | sort -rn | printTable "FreeSWITCH event streams closing"
        grep -a "failed to decode packet from" $TMP_FILE | cut -d' ' -f3,13,14,15,17 | printTable "FreeSWITCH event stream decoding errors"

done
