#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|stepswitch_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Route Request Replays" $(countMatches "sending SIP authentication reply")
        printStat "Local Resource Requests" $(countMatches "attempting to find local resources")
        printStat "Global Resource Requests" $(countMatches "attempting to find global resources")
        printStat "No Matching Resource" $(countMatches "failed to find matching resource")
        printStat "Local Fetch Failures" $(countMatches "unable to fetch local resources") 
        printStat "Global Fetch Failures" $(countMatches "unable to fetch global resources") 
        printStat "Bad Rules" $(countMatches "bad rule")
        printStat "Outbound Audio Requests" $(countMatches "received outbound audio resource request")
        printStat "Outbound SMS Requests" $(countMatches "received outbound sms ")
        printStat "Shortdial Corrections" $(countMatches "corrected shortdial")
        printStat "No Outbound Resources" $(countMatches "no endpoints found")
        printStat "Outbound Bridge Attempts" $(countMatches "sent bridge command to")
        printStat "Outbound Bridge Success" $(countMatches "outbound request successfully completed")
        printStat "Outbound Bridge Failed" $(countMatches "resources for outbound request failed")
        printStat "Outbound Bridge Errors" $(countMatches "error during outbound request")
        printStat "Outbound Bridge Timeout" $(countMatches "attempt to connect to resources timed out")
        printStat "Outbound Emergency " $(countMatches "endpoints contain an emergency resource")
        printStat "Outbound Emergency Block" $(countMatches "terminating attempted emergency bridge")
        printStat "Local Attempts" $(countMatches "sent local extension command") 
        printStat "Local Bridge Success" $(countMatches "local extension request successfully completed")
        printStat "Local Bridge Errors" $(countMatches "error during outbound request")
        printStat "Local Bridge Timeout" $(countMatches "attempt to connect to resources timed out")
        printStat "Originate Attempts" $(countMatches "sent originate command") 
        printStat "Originate Success" $(countMatches "originate request successfully completed")
        printStat "Originate Failed" $(countMatches "originate request failed")
        printStat "Originate Errors" $(countMatches "error during originate request")
        printStat "Originate Timeout" $(countMatches "attempt to connect to resources timed out")
        printStat "Inbound Relays" $(countMatches "relaying route request")
        printStat "Inbound Failues" $(countMatches "unable to determine account")
        printStat "Inbound Blacklist" $(countMatches "is blacklisted")

        zgrep "relaying route request" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Inbound Requests"
        zgrep "attempting to find" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Outbound Requests"
done

