#!/bin/bash

cd `dirname $0`

export AGGREGATE_STATS=1

. ./klap-utils.sh

for FILE in $(listFiles $@); do
	echo $FILE
	fileInfo "$FILE"
	for ANALYZER in analyzers/klap-*; do
		./$ANALYZER $FILE
	done
	echo
done
