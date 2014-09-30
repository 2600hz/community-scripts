#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
	createTmpFile "couch_util"

	isTmpFileEmpty
	if [ $? != 0 ]; then
		continue
	fi

        statHeader

	zgrep "unformatted error" $TMP_FILE | rev | cut -d " " -f 1 | rev | sort | uniq -c | sort -nr | head | printTable "Unformatted Errors"
        zgrep "gateway_timeout" $FILE | wc -l | printTable "Number of gateway timeouts"
done
