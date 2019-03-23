#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|hon_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

	printStat "Rates Found" $(countMatches "using rate")
	printStat "Missing Rates" $(countMatches "no (results|rates)")
	printStat "Lookup Errors" $(countMatches "rate lookup error")

	zgrep -o "using rate definition .*$" $TMP_FILE | cut -c22- | sort | uniq -c | sort -nr | head | printTable "Most Frequent Rates"
done
