#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|registrar_|\|reg_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Authentication Reply" $(countMatches "SIP authentication reply")
        printStat "Authentication Error" $(countMatches "SIP authentication error")
        printStat "Disabled Credentials" $(countMatches "rejecting authn for disabled")
        printStat "Lookup By IP" $(countMatches "looking up IP")
        printStat "Ignored IP Realm" $(countMatches "realm is an IP address")
        printStat "Credential Lookup Errors" $(countMatches "failed to look up SIP")
        printStat "Route Request Replays" $(countMatches "route req was missing account information")
        printStat "Failed Route Request" $(countMatches "not replaying route req")

        zgrep "auth failure for" ${TMP_FILE}  | rev | cut -d " " -f 2 | rev | sort 2> /dev/null | uniq -c | sort -rn 2> /dev/null | head | printTable "Top User Failures"
        zgrep "trying to authenticate" ${TMP_FILE} | rev | cut -d " " -f 1 | rev | sort 2> /dev/null | uniq -c | sort -rn 2> /dev/null | head | printTable "Top Requested Users"
        zgrep "trying to authenticate" ${TMP_FILE} | rev | cut -d " " -f 1 | cut -d "@" -f 1 | rev | sort 2> /dev/null | uniq -c | sort -rn 2> /dev/null | head | printTable "Top Realms"
        zgrep "trying to authenticate" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Authentication Requests"
done
