#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
	zgrep -E "(GET|PUT|POST|DELETE) [^\s]+ " "${FILE}" | grep -v "GET / " > "${TMP_FILE}"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

	printStat "Number of Errors" `zgrep "error_report" "${FILE}" | wc -l`
	printStat "Number of Stacktraces" `zgrep "Stacktrace" "${FILE}" | wc -l`

	zgrep -Eo "GET|PUT|POST|DELETE" ${TMP_FILE} | sort -nr | uniq -c | printTable "HTTP Methods"
        zgrep -Eo "(GET|PUT|POST|DELETE) [^\s]+ " "${TMP_FILE}" | cut -d ' ' -f 2 | sort | uniq -c | sort -nr | head | printTable "Most Frequent Requests"
        zgrep -Eo "(GET|PUT|POST|DELETE) [^\s]+ [0-9]+ ok [0-9]+$" "${TMP_FILE}" | awk '{ print $5, $1, $2 }' | sort -nr | head | printTable "Largest Response Times"
        zgrep -Eo "(GET|PUT|POST|DELETE) [^\s]+ " "${TMP_FILE}" | cut -d ' ' -f 3 | sort | uniq -c | sort -nr | printTable "Response Codes"
        zgrep -Eo " [0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3} " "${TMP_FILE}" | sort | uniq -c | sort -nr | head | printTable "Most Frequent Requestors"
	zgrep -Eo "\w+\, \w+ \w+ \w+ [0-9]+:[0-9]+" "${TMP_FILE}" | sort | uniq -c | sort -nr | head | printTable "Most Active Times"

        if [ -d "/srv" ]; then
            find /srv/db/shards/ -size +50M -exec ls -hl {} \; | awk '{ print $5, $9 }' | sort -rn | head -25 | printTable "Largest Shards"
        fi
done
