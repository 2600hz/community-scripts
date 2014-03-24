#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        createExactTmpFile "\+1[2-9][0-9]{2}[2-9][0-9]{6}"

        isTmpFileEmpty
        if [ $? != 0 ]; then
                continue
        fi

        statHeader

        printStat "US Numbers" $(wc -l $TMP_FILE)
	printStat "Toll-Free Numbers" $(countMatches "\+1(800|888|877|866|855)[0-9]{7}")
	printStat "Toll Numbers" $(countMatches "\+1900[0-9]{7}")
	printStat "Caribbean" $(countMatches "\+1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)[0-9]{7}")
	cat $TMP_FILE | sort | uniq -c | sort -nr | head | printTable "Most Frequent Numbers"
done
