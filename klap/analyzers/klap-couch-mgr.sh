#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
	createTmpFile "kz_couch_util"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

	zgrep "unformatted error" $TMP_FILE | rev | cut -d " " -f 1 | rev | sort | uniq -c | sort -nr | head | printTable "Unformatted Errors"
        zgrep "gateway_timeout" $FILE | wc -l | printTable "Number of gateway timeouts"
        zgrep "connection_timeout" $FILE | wc -l | printTable "Number of connection timeouts"
        zgrep "network_unreachable" $FILE | wc -l | printTable "Number of network unreachables"
        zgrep "tcp_closed" $FILE | wc -l | printTable "Number of Closed TCP sockets"
        zgrep "system limit" $FILE | wc -l | printTable "Number of system limits met"
        zgrep "econnrefused" $FILE | wc -l | printTable "Number of refused connections"
        zgrep "server_timeout" $FILE | wc -l | printTable "Number of CouchDB timeouts"
        zgrep "server error" $FILE | wc -l | printTable "Number of CouchDB errors"

done
