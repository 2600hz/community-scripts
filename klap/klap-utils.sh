#!/bin/bash

# KLAP kazoo-log-analysis-program

SCRIPTNAME="$(basename $0)"
TMP_FILE="/tmp/stat-builder.tmp"

printStat() {
	printf "  %-25s: %s\n" "$1" "$2"
}

printTable() {
	echo "  $1"
	while read LINE
	do
		KEY=`echo $LINE | cut -d ' ' -f 1`
		VALUE=`echo $LINE | cut -d ' ' -f 2-`
		if [ "$KEY" == "$VALUE" ]; then
			echo "    $KEY"
		else
			printf "    %-6s %s\n" "$KEY" "$VALUE"
		fi
	done
}

statHeader() {
	if [ $# -eq 0 ]; then
		local _FILE="$FILE"
	else
		local _FILE="$1"
	fi

	if [ -z "$AGGREGATE_STATS" ]; then
		echo "$_FILE"
		fileInfo "$_FILE"
		echo
	else
		local NAME="${SCRIPTNAME%.*}"
		local NAME="${NAME/klap-/}"
		echo
		echo " ${NAME^^}"
	fi
}

createTmpFile() {
	if [ $# -eq 1 ]; then
		local _FILE="$FILE"
		local _PATTERN="$1"
	else
		local _FILE="$1"
		local _PATTERN="$2"
	fi

	zgrep -E "$_PATTERN" "$_FILE" > "$TMP_FILE"
}

createExactTmpFile() {
	if [ $# -eq 1 ]; then
		local _FILE="$FILE"
		local _PATTERN="$1"
	else
		local _FILE="$1"
		local _PATTERN="$2"
	fi

	zgrep -Eo "$_PATTERN" "$_FILE" > "$TMP_FILE"
}

isTmpFileEmpty() {
	if [ -s "$TMP_FILE" ]; then
		return 0;
	else
		return 1
	fi
}

countMatches() {
	echo `zgrep -Ec "$1" "$TMP_FILE"`
}

fileInfo() {
	if [ "${1##*.}" = "gz" ]; then
        START_TIMEDATE=`zcat $1 | head -n 1 | grep -Po "^\w+\s+\d+\s+\d+:\d+:\d+"`
        END_TIMEDATE=`zcat $1 | tail -1 | grep -Po "^\w+\s+\d+\s+\d+:\d+:\d+"`
	else
        START_TIMEDATE=`cat $1 | head -n 1 | grep -Po "^\w+\s+\d+\s+\d+:\d+:\d+"`
        END_TIMEDATE=`cat $1 | tail -1 | grep -Po "^\w+\s+\d+\s+\d+:\d+:\d+"`
	fi

        START_EPOCH=`date -d "$START_TIMEDATE" "+%s"`
        END_EPOCH=`date -d "$END_TIMEDATE" "+%s"`
        DURATION=$(expr $END_EPOCH - $START_EPOCH)

        printStat 'Start' "$START_TIMEDATE"
        printStat 'End' "$END_TIMEDATE"
        printStat 'Duration' "${DURATION}s"
}

listFiles() {
	echo `printf "%s\n" $@ | sort -V`
}
