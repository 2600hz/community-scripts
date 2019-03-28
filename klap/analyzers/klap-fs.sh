#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
	createTmpFile "mod_amqp|kazoo_|mod_kazoo"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

        vsn="$(zgrep -m1 -Eo "FreeSWITCH Version (.+)" $1)"
        if [ ! -z "$vsn" ]; then
            echo "  $vsn"
            echo
        fi

        # AMQP-related connectivity
        zgrep "failed to send heartbeat on connection" $TMP_FILE | cut -d' ' -f2,5,12,13 | sort | uniq -c | printTable "Failed AMQP connections"
        zgrep "opened socket connection to AMQP broker" $TMP_FILE | cut -d' ' -f2,5,12 | sort | uniq -c | head | printTable "Connections to AMQP"

        # ecallmgr-related connectivity
        zgrep "Connected to epmd and published erlang cnode" $TMP_FILE | grep -Po " \d{2}:\d{2}:\d{2}" | head | printTable "KAZOO C-Node ready"
        zgrep "New erlang connection to node" $TMP_FILE | cut -d' ' -f2,10,11 | sort  | printTable "Connections from ecallmgr"

        # ACL reloads
        zgrep "configuration name=\"acl.conf\"" $TMP_FILE | grep -Po " \d{2}:\d{2}:\d{2}" | uniq | printTable "ACL reloads"
done
