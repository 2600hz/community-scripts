#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        createExactTmpFile "\|.+\|"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader	

        printStat "Unique Call-IDs" `cat $TMP_FILE | sort | uniq | wc -l`
done
