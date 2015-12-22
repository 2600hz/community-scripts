#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
	createExactTmpFile "\|\w+:[0-9]+ \(<[0-9]+\.[0-9]+\.[0-9]+>\) "

        statHeader

	printStat "Unique PIDs" `cat $TMP_FILE | sort | uniq | wc -l`
	zgrep -Eo "<[0-9]+\.[0-9]+\.[0-9]+>" $TMP_FILE | sort 2> /dev/null | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent PIDs"
	zgrep -Eo "\|\w+:[0-9]+ " $TMP_FILE | cut -c2- | sort 2> /dev/null | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Lines of Code"
	zgrep -Eo "^\w+ \w+ [0-9]+:[0-9]+" "${FILE}" | sort 2> /dev/null | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Active Times"
        zgrep -Eo "2600hz\[[0-9]+\]" "${FILE}" | sort 2> /dev/null | uniq | printTable "OS Processes"
done
