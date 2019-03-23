#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|trunkstore_|\|ts_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Offnet Requests" $(countMatches "call began from outside the network")
        printStat "Onnet Requests" $(countMatches "call began on the network")
        printStat "Non-trunkstore Route Requests" $(countMatches "insufficient information available to lookup routing, ignoring")
done
