#!/bin/bash

cd `dirname $0`

. ../klap-utils.sh

for FILE in $(listFiles $@); do
        statHeader

	zgrep "abnormal call termination" ${FILE}  | rev | cut -d " " -f 1 | rev | sort | uniq -c  | sort -nr | printTable "Abnormal Hangup Causes"
done
