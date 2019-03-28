#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
	createTmpFile " mod_amqp"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

        zgrep "failed to send heartbeat on connection" $TMP_FILE | grep -Po "([\s\w]+)\)?$" | uniq -c | sort -nr 2> /dev/null | head | printTable "Failed Heartbeat Reasons"
done
