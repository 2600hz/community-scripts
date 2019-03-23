#!/bin/bash

. $(dirname $(dirname $0))/klap-utils.sh

for FILE in $(listFiles $@); do
        createTmpFile "\|sysconf_"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "Gets" $(countMatches "received sysconf get ")
        printStat "Sets" $(countMatches "sysconf_set")
done
