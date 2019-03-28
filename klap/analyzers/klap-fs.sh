#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
	createTmpFile " mod_amqp|kazoo_|mod_kazoo"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

        zgrep "failed to send heartbeat on connection" $TMP_FILE | grep -Po "([\s\w]+)\)?$" | uniq -c | sort -nr 2> /dev/null | head | printTable "Failed Heartbeat Reasons"
        zgrep "Connected to epmd and published erlang cnode" $TMP_FILE | grep -Po " \d{2}:\d{2}:\d{2}" | head | printTable "KAZOO C-Node ready"
        zgrep "opened socket connection to AMQP broker" $TMP_FILE | cut -d' ' -f2,12 | sort | uniq -c | head | printTable "Connections to AMQP"
        zgrep "configuration name=\"acl.conf\"" $TMP_FILE | grep -Po " \d{2}:\d{2}:\d{2}" | uniq | printTable "ACL reloads"
done
