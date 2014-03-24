#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|jonny5_|\|j5_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Allowed Calls" $(countMatches "call is authorized as")
        printStat "Disallowed call" $(countMatches "call is unauthorize due to")
        printStat "Emergency Exceptions" $(countMatches "allowing emergency call")
        printStat "Toll-free Exceptions" $(countMatches "allowing outbound tollfree call")
done
