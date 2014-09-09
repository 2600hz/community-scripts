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

        printStat "Found Credentials" $(countMatches "SIP authentication reply")
        printStat "Missing Credentials" $(countMatches "SIP authentication error")
        printStat "Disabled Credentials" $(countMatches "rejecting")
        printStat "Credential Lookup Errors" $(countMatches "failed to look up SIP")
        printStat "Found Auth-by-IP" $(countMatches "replaying route_req")
        printStat "Missing Auth-by-IP" $(countMatches "no entry in sip_auth")
        printStat "Auth-by-IP Lookup Errors" $(countMatches "failed to lookup by ip")

        zgrep "auth failure for" ${TMP_FILE}  | rev | cut -d " " -f 2 | rev | sort | uniq -c | sort -rn | head | printTable "Highest failures"
done
