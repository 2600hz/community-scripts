#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|callflow_|\|cf_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Route Requests" $(countMatches "received a request asking if callflows can route this call")
        printStat "Lookup Errors" $(countMatches "unable to find callflow")
        printStat "Denied 'no_match'" $(countMatches "nomatch for a unauthorized call")
        printStat "No Audio Service" $(countMatches "does not have audio service")
        printStat "Restrictions Enforced" $(countMatches "endpoint is restricted from")
        printStat "Park Responses" $(countMatches "park response")
        printStat "Route Wins" $(countMatches "received a route win")
        printStat "Route Win Timeouts" $(countMatches "didn't received a route win")
        printStat "On-Net Calls" $(countMatches "inception on-net")
        printStat "Call Resumes" $(countMatches "received call resume")
        printStat "Executer Started" $(countMatches "beginning to process the call")
        printStat "Executer Reset" $(countMatches "has been reset")
        printStat "Executer Branched" $(countMatches "has been branched")
        printStat "Executer Transferred" $(countMatches "has been transferred")
        printStat "Executer Usurped" $(countMatches "has been usurped")
        printStat "Executer Stoped" $(countMatches "execution has been stopped")
        printStat "Call-ID Updated" $(countMatches "updating callid")
        printStat "Spawn Failures" $(countMatches "failed to spawn")
        printStat "Task Listeners" $(countMatches "started event listener for cf_task")
        printStat "MWI Query Replies" $(countMatches "replying to mwi query")
        printStat "MWI Updates" $(countMatches "updating MWI for")
        printStat "MWI Probe Replies" $(countMatches "replying to mwi presence prob")

        zgrep "moving to action" $TMP_FILE | egrep -o "'cf_.+'$" | sort | uniq -c | sort -nr | head | printTable "Most Frequent Callflow Modules"
        zgrep "received a request asking if callflows can route this call" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Route Requests"
        zgrep "received a route win" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Route Wins"
        zgrep "execution has been stopped" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Execution Stop"
	zgrep -Eo "callflow [A-Za-z0-9]+ in [A-Za-z0-9]+ satisfies request" $TMP_FILE | cut -d' ' -f4 | sort 2> /dev/null | uniq -c |  sort -nr 2> /dev/null | head | printTable "Top Accounts"
	zgrep -Eo "callflow [A-Za-z0-9]+ in [A-Za-z0-9]+ satisfies request" $TMP_FILE | cut -d' ' -f2 | sort 2> /dev/null | uniq -c |  sort -nr 2> /dev/null | head | printTable "Top Callflows"
done
