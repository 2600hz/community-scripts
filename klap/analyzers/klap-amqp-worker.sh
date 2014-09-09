#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

normalize() {
	while read LINE; do
		echo "$(expr $LINE / 1000000) seconds"
	done
}

for FILE in $(listFiles $@); do
	createTmpFile "\|wh_amqp_worker"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

	printStat "Requests" $(countMatches "published request")
	printStat "Unexpected Message" $(countMatches "received unexpected message")
	printStat "Timeouts" $(countMatches "request timeout")
	printStat "Failed Publishes" $(countMatches "failed to publish request")

	zgrep -Eo " took [0-9]+ micro to return" $TMP_FILE | grep -Eo "[0-9]+" | normalize | sort -nr | uniq -c | printTable "Lookup Histogram"
done
