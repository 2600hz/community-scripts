#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
    zgrep -Eo "(GET|PUT|POST|DELETE) /.*" "${FILE}" | grep -v "GET / " > "${TMP_FILE}"

    isTmpFileEmpty
    if [ $? != 0 ]; then
        continue
    fi

    statHeader

    printStat "Number of Errors" `zgrep "error_report" "${FILE}" | wc -l`
    printStat "Number of Stacktraces" `zgrep "Stacktrace" "${FILE}" | wc -l`

    cut -d ' ' -f 1 "${TMP_FILE}" | sort -nr | uniq -c | sort -nr | printTable "HTTP Methods"
    cut -d ' ' -f 3 "${TMP_FILE}" | sort | uniq -c | sort -nr | printTable "Response Codes"

    cut -d ' ' -f 2 "${TMP_FILE}" | egrep -v "^/account%2F|^/numbers%2F|/_" | cut -d'/' -f 2 | sort | uniq -c | sort -nr | printTable "Kazoo DB Requests"
    cut -d ' ' -f 2 "${TMP_FILE}" | egrep -v "^/account%2F|^/numbers%2F|/_" | sort -t'/' -k2,2 | uniq -c | sort -rn | head -n 25 | printTable "Kazoo DB Documents"

    cut -d ' ' -f 2 "${TMP_FILE}" | grep "^/numbers%2F" | grep -v "/_" | cut -d'/' -f 2 | sort | uniq -c | sort -nr | head -n 25 | printTable "Number DB Requests"
    cut -d ' ' -f 2 "${TMP_FILE}" | grep "^/numbers%2F" | grep -v "/_" | sort -t'/' -k2,2 | uniq -c | sort -rn | head -n 25 | printTable "Number Documents"

    cut -d ' ' -f 2 "${TMP_FILE}" | grep "^/account%2F" | grep -v "/_" | cut -d'/' -f 2 | sort | uniq -c | sort -nr | head -n 25 | printTable "Account DB Requests"
    cut -d ' ' -f 2 "${TMP_FILE}" | grep "^/account%2F" | grep -v "/_" | sort -t'/' -k2,2 | uniq -c | sort -rn | head -n 25 | printTable "Account Documents"

    cut -d ' ' -f 2 "${TMP_FILE}" | sort | uniq -c | sort -nr | head -n 25 | printTable "Most Frequent Requests"

    awk '{ print $5, $1, $2 }' "${TMP_FILE}" | sort -nr | head -n 25 | printTable "Largest Response Times"

    zgrep -E "(GET|PUT|POST|DELETE) /.*" "${FILE}" | grep -v "GET / " > "${TMP_FILE}"
    egrep -o " [0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3} " "${TMP_FILE}" | sort | uniq -c | sort -nr | head -n 25 | printTable "Most Frequent Requestors"

    zgrep -Eo "('GET'|'PUT'|'DELETE'|'POST') /.*" "${FILE}" | grep -v "GET / " > "${TMP_FILE}"

    egrep -o "/_design/.+/_view/.+ " "${TMP_FILE}" | cut -d '/' -f 3 | sort | uniq -c | sort -rn | head -n 25 | printTable "Design Documents"
    egrep -o "/_design/.+/_view/.+ " "${TMP_FILE}" | sed 's|?.*$||g' | cut -d'/' -f3- | sort -t'/' -k3,3 | uniq -c | sort -rn | head -n 25 | printTable "Views"

    egrep -o "_compact|_view_cleanup" "${TMP_FILE}" | sort | uniq -c | sort -rn | printTable "Compaction"
    egrep "_compact|_view_cleanup" "${TMP_FILE}" | cut -d'/' -f2 | sort | uniq -c | sort -rn | head -n 25 | printTable "DB Compaction"

    zgrep -E "('GET'|'PUT'|'DELETE'|'POST') /.*" "${FILE}" | grep -v "GET / " > "${TMP_FILE}"

    egrep -o "\w+\, \w+ \w+ \w+ [0-9]+" "${TMP_FILE}" | uniq -c | sort -rn | head -n 25 | printTable "Most Active"
    grep "GET" "${TMP_FILE}" | egrep -o "\w+\, \w+ \w+ \w+ [0-9]+" | uniq -c | sort -rn | head -n 25 | printTable "Most Active GET"
    grep "PUT" "${TMP_FILE}" | egrep -o "\w+\, \w+ \w+ \w+ [0-9]+" | uniq -c | sort -rn | head -n 25 | printTable "Most Active PUT"
    grep "POST" "${TMP_FILE}" | egrep -o "\w+\, \w+ \w+ \w+ [0-9]+" | uniq -c | sort -rn | head -n 25 | printTable "Most Active POST"
    grep "DELETE" "${TMP_FILE}" | egrep -o "\w+\, \w+ \w+ \w+ [0-9]+" | uniq -c | sort -rn | head -n 25 | printTable "Most Active DELETE"
    egrep "_compact|_view_cleanup" "${TMP_FILE}" | egrep -o "\w+\, \w+ \w+ \w+ [0-9]+" | uniq -c | sort -rn | head -n 25 | printTable "Most Active Compaction"
    
    if [ -d "/srv/db/shards/" ]; then
        find /srv/db/shards/ -size +50M -exec ls -hl {} \; | awk '{ print $5, $9 }' | sort -rn | head -25 | printTable "Largest Shards"
    fi
done
