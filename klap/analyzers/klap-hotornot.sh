#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "hon_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

	printStat "DIDs matched to rate" $(countMatches "using rate")
	printStat "DIDs unmatched to rate" $(countMatches "no rates found for")
	printStat "Rate Lookup Errors" $(countMatches "rate lookup error")

	zgrep "using rate .* for" $TMP_FILE | grep -Po "(?<=rate )\w+" | sort | uniq -c | sort -nr | head | printTable "Most Frequent Rates"
        zgrep "searching for prefixes for " $TMP_FILE | grep -Eo " \+?[0-9]{5,}" | sort | uniq -c | sort -nr | head | printTable "Most Searched Numbers"
done
