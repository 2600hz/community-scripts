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

        printStat "Outbound Bridges" $(countMatches "sent bridge command to")
        printStat "Outbound Originates" $(countMatches "sent originate command")
        printStat "Outbound Loopbacks" $(countMatches "sent local extension command")
        printStat "Outbound Failures" $(countMatches "no available resources for")
        printStat "Inbound Relays" $(countMatches "relaying route request")
        printStat "Inbound Failues" $(countMatches "unable to determine account")
done

