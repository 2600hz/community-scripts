#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for F in $(listFiles $@); do
    FILE=$(realpath $F)
    CWD=$(pwd)

	createTmpFile "\|wh_amqp_| Channel |amqp_|kz_amqp_"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

        printStat "New Connections" $(countMatches "connection .+ is now available")
        printStat "Lost Connections" $(countMatches "connection .+ is no longer available")
        printStat "Terminated Connections" $(countMatches "connection .+ went down")
        printStat "Connection Auth Failure" $(countMatches "amqp authentication failure")
        printStat "Failed Connections" $(countMatches "failed to connect")
        printStat "Connection Errors" $(countMatches "unhandled case on connect")
        printStat "Connection Execptions" $(countMatches "exception connecting to")
        printStat "Invalid Connections" $(countMatches "assuming connection is invalid")
	printStat "New Channels" $(countMatches "started amqp_channel")
        printStat "Closed Unused Channels" $(countMatches "unused channel .+ on .+ went down")
        printStat "Closed Floating Channels" $(countMatches "floating channel .+ on .+ went down")
        printStat "Closed Sticky Channels" $(countMatches "sticky channel .+ on .+ went down")
        printStat "Closed Channels" $(countMatches "closed amqp channel")
        printStat "New Consumers" $(countMatches " created consumer ")
	printStat "New Queues" $(countMatches " declared queue ")
	printStat "New Bindings" $(countMatches " bound ")

        printStat "Assigned Consumers" $(countMatches "assigned existing consumer")
        printStat "Assigned Channels" $(countMatches "assigned existing channel")
        printStat "Assigned New Channel" $(countMatches "assigned consumer .+ new channel")
        printStat "Reassignments" $(countMatches "reassigning consumer")
        printStat "Waiting on Sticky" $(countMatches "waiting on sticky AMQP channel")
        printStat "Waiting on Float" $(countMatches "waiting on AMQP channel")

        printStat "Replayed Commands" $(countMatches "replaying previous AMQP commands")
        printStat "Replayed Command Failures" $(countMatches "replayed command failed")
        printStat "Command Exceptions" $(countMatches "amqp command exception")
        printStat "Unexpected Command Result" $(countMatches "unexpected AMQP command result")
	printStat "Dropped Methods" $(countMatches " dropping method ")
	printStat "Dropped Payloads" $(countMatches "dropping payload ")
	printStat "Short Lived Channels" $(countMatches "short lived channel")
	printStat "Published" $(countMatches " published to ")

	zgrep -E 'published\(' $TMP_FILE | grep -Eo "routing key [^\)]+" | cut -c12- | sort 2> /dev/null | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Routing Keys"
	zgrep -E 'published\(' $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Publishes"
	grep "started amqp_channel" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent New Channels"
done
