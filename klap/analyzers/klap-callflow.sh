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

        printStat "Park Responses" $(countMatches "sent park response")
        printStat "Denied 'no_match'" $(countMatches "nomatch for a unauthorized call")
        printStat "Lookup Errors" $(countMatches "unable to find callflow")
        printStat "Route Wins" $(countMatches "received a route win")
        printStat "Route Win Errors" $(countMatches "callflow during second lookup")
        printStat "MWI Query Replies" $(countMatches "replying to mwi query")
        printStat "MWI Updates" $(countMatches "updating MWI for")
        printStat "MWI Probe Replies" $(countMatches "replying to mwi presence prob")
done
