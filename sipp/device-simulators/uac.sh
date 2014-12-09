#!/bin/bash
cd `dirname $0`

if [ -z "$1" -o -z "$2" ]; then
    echo "$0 <target-ip> <dialed-number> [interface-ip]"
    exit 1
fi

TARGET="$1"
shift
NUMBER="$1"
shift

if [ -z "$1" ]; then
    INTERFACE="${TARGET}"
else
    INTERFACE="$1"
    shift
fi


echo "# sipp -inf callers.csv -sf uac.xml -i ${INTERFACE} -p 7653 ${TARGET} -s ${NUMBER} -d 15000 $@"
sipp -inf callers.csv -sf uac.xml -i ${INTERFACE} -p 7653 ${TARGET} -s ${NUMBER} -d 15000 $@
