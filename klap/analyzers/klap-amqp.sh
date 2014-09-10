#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
	createTmpFile "\|wh_amqp_| Channel "

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

	printStat "Published" $(countMatches " published to ")
	printStat "Channels" $(countMatches " create channel ")
	printStat "Queues" $(countMatches " declared queue ")
	printStat "Consumers" $(countMatches " created consumer ")
	printStat "Bindings" $(countMatches " bound ")
	printStat "Dropped Methods" $(countMatches " dropping method ")
	printStat "Short Lived Channels" $(countMatches "short lived channel")
        printStat "Connections" $(countMatches "connection to the AMQP broker")

	zgrep " published to " $TMP_FILE | grep -Eo "routing key [^\)]+" | cut -c12- | sort | uniq -c | sort -nr | head | printTable "Most Frequent Routing Keys"
done
