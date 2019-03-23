#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

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

	printStat "New Workers" $(countMatches "starting amqp worker")
	printStat "Poolboy Exceptions" $(countMatches "poolboy exception")
        printStat "Flowcontrol Enforced" $(countMatches "flow control")
        printStat "Pending Queue" $(countMatches "prior to queue creation")

        printStat "Collections" $(countMatches "attempting to collect")
        printStat "Collection Timeouts" $(countMatches "req timeout for call_collect")

        printStat "Message Publishes" $(countMatches "published message")
	printStat "Failed Message Publishes" $(countMatches "failed to publish message")
	printStat "Bad Publisher Fun" $(countMatches "publisher fun returned")
        printStat "Message Publisher Errors" $(countMatches "error when publishing")
        printStat "Returned Messages" $(countMatches "returned from the broker")
        printStat "Late Message ACKs" $(countMatches "confirm message was returned from the broker but it was too late")
        printStat "Message ACKs" $(countMatches "ack message was returned")
        printStat "Message NACKs" $(countMatches "nack message was returned")

        printStat "Request Publishes" $(countMatches "published request")
        printStat "Failed Request Publishes" $(countMatches "failed to send request")
	printStat "Failed Requests" $(countMatches "request failed")
        printStat "Invalid Respones" $(countMatches "response failed validator")
        printStat "Invalid Defered Respones" $(countMatches "invalid resp as it was deferred")
        printStat "Deferred Respones" $(countMatches "waiting for primary response")
        printStat "Only Deferred Respones" $(countMatches "only received defered response")
	printStat "Unexpected Message" $(countMatches "received unexpected message")
        printStat "Criteria Met" $(countMatches "criteria for the client")
        printStat "Negative Threashold" $(countMatches "negative response threshold reached")
        printStat "Completed Requests" $(countMatches "response for msg id")
	printStat "Request Timeouts" $(countMatches "request timeout")

	zgrep -Eo " took [0-9]+ micro to return" $TMP_FILE | grep -Eo "[0-9]+" | normalize | sort -nr 2> /dev/null | uniq -c | printTable "Lookup Histogram"
        zgrep "published request" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | printTable "Most Frequent Request Rates"
        zgrep "failed" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Failure Rates"
        zgrep "timeout" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Timeout Rates"
done
