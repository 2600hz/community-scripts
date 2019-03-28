#!/bin/bash

set -e

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|callflow_|\|cf_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Route Requests" $(countMatches "asking if callflows can route")
        printStat "Lookup Errors" $(countMatches "unable to find callflow")
        printStat "Denied 'no_match'" $(countMatches "only available callflow is a nomatch for a unauthorized call")
        printStat "No Audio Service" $(countMatches "does not have audio service")
        printStat "Restrictions Enforced" $(countMatches "endpoint is restricted from making this call, terminate")
        printStat "Park Responses" $(countMatches "sending park response")
        printStat "Route Wins" $(countMatches "callflow has received a route win")
        printStat "Route Win Timeouts" $(countMatches "callflow didn't received a route win, exiting")
        printStat "On-Net Calls" $(countMatches "inception on-net")
        printStat "Call Resumes" $(countMatches "received call resume")
        printStat "Executer Started" $(countMatches "executing callflow ")
        printStat "Executer Reset" $(countMatches "has been reset")
        printStat "Executer Branched" $(countMatches "callflow has been branched")
        printStat "Executer Transferred" $(countMatches "callflow execution has been transferred")
        printStat "Executer Usurped" $(countMatches "the call has been usurped by an external process")
        printStat "Executer Stopped" $(countMatches "callflow execution has been stopped")
        printStat "Call-ID Updated" $(countMatches "updating callid to")
        printStat "Spawn Failures" $(countMatches "failed to spawn")
        printStat "Task Listeners" $(countMatches "started event listener for cf_task")
        printStat "MWI Query Replies" $(countMatches "replying to mwi query")
        printStat "MWI Updates" $(countMatches "updating MWI for")
        printStat "MWI Probe Replies" $(countMatches "replying to mwi presence prob")

        zgrep "moving to action" $TMP_FILE | egrep -o "'cf_.+'$" | sort | uniq -c | sort -nr | head | printTable "Most Frequent Callflow Modules"
        zgrep "asking if callflows can route the call to" $TMP_FILE | grep -Eo " \+?[0-9]+$" | sort -n | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Calls To"
        zgrep "callflow has received a route win" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | sort | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent Route Wins Received"
        zgrep "callflow execution has been stopped" $TMP_FILE | grep -Eo "^\w+ \w+ [0-9]+:[0-9]+" | sort | uniq -c | sort -nr 2> /dev/null | head | printTable "Most Frequent CF Execution Stop"
	zgrep -Eo "callflow [A-Za-z0-9]+ in [A-Za-z0-9]+ satisfies request" $TMP_FILE | awk 'BEGIN{FS=" "} {print $4, $2}' | sort | uniq -c |  sort -nr 2> /dev/null | head | printTable "Top Account Callflows"

done
