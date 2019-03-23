#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|jonny5_|\|j5_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Account Authorized Calls" $(egrep "account [A-Za-z0-9]+ authorized channel" $TMP_FILE | wc -l)
        printStat "Reseller Authorized Calls" $(egrep "reseller [A-Za-z0-9]+ authorized channel" $TMP_FILE | wc -l)
        printStat "Account Denied Calls" $(egrep "account [A-Za-z0-9]+ denied channel" $TMP_FILE | wc -l)
        printStat "Reseller Denied Calls" $(egrep "reseller [A-Za-z0-9]+ denied channel" $TMP_FILE | wc -l)
        printStat "Disabled Account Calls" $(egrep "account [A-Za-z0-9]+ is disabled" $TMP_FILE | wc -l)
        printStat "Disabled Limits Calls" $(countMatches "limits are disabled")
        printStat "Local Resources Processed" $(countMatches "authz_local_resources enabled")
        printStat "Accounts Found by IP" $(countMatches "found account auth'd by IP")
        printStat "Unknown Account IDs" $(countMatches "unable to determine account id")
        printStat "Emergency Exceptions" $(countMatches "allowing emergency call")
        printStat "Toll-free Exceptions" $(countMatches "allowing outbound tollfree call")
        printStat "Channel Disparities" $(countMatches "channel disparity with ecallmgr")
done
