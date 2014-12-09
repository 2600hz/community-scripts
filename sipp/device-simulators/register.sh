#!/bin/bash
cd `dirname $0`

if [ -z "$1" ]; then
    echo "$0 <target-ip> [interface-ip]"
    exit 1
fi

TARGET="$1"
shift

if [ -z "$1" ]; then
    INTERFACE="${TARGET}"
else
    INTERFACE="$1"
    shift
fi

echo "# sipp -inf callees.csv -sf register.xml -m 1 -l 1 -r 1 -d 1000 -s 5000 -i ${INTERFACE} -p 7653 ${TARGET} $@"
sipp -inf callees.csv -sf register.xml -m 1 -l 1 -r 1 -d 1000 -s 5000 -i ${INTERFACE} -p 7653 ${TARGET} $@
