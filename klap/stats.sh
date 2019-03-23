#!/bin/bash

export AGGREGATE_STATS=1

. $(dirname $0)/klap-utils.sh

for FILE in $(listFiles $@); do
	echo $FILE
	fileInfo "$FILE"
	for ANALYZER in $(dirname $0)/analyzers/klap-*; do
            echo 'running: ' $ANALYZER $FILE
	    $ANALYZER $FILE
	done
	echo
done
