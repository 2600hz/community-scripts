#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|omnip_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Updates" $(countMatches "ending update")
        printStat "Subscriptions" $(countMatches " subscribe ")
        printStat "Resubscriptions" $(countMatches " re-subscribe ")
        printStat "Subscription Miss" $(countMatches " no subscriptions ")
        printStat "Search Requests" $(countMatches "searching for subs for")
done
