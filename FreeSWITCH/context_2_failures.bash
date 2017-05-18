#!/bin/bash

## Usage: ./context_2_failures.bash /var/log/freeswitch/kazoo_debug.log
##            DID |                   IP | Call-ID
##     1234567890 |     10.10.10.10:5060 | abc123@10.10.10.10
##    +1345678901 |     20.20.20.20:5060 | 123789128371298@20.20.20.20
##    +1456789012 |     30.30.30.30:5060 | 291834576asdfk@30.30.30.30
##
## Pipe to count numbers that failed:
## ./context_2_failures.bash /var/log/freeswitch/kazoo_debug.log | awk -F ' +' '{print $2}' | sort | uniq -c | sort

MD5=$(md5sum $1 | cut -d ' ' -f 1)
TMP_FILE="/tmp/$MD5.${1##*.}"

if [ ! -f $TMP_FILE ]; then
    cp $1 $TMP_FILE
fi

function print_column {
    printf "%15s | %20s | %s\n" $1 $2 $3
}

function find_did {
    local call_id=$1
    local file=$2

    local did=$(zgrep "^$call_id .* New Channel sofia/sipinterface_1" $file | cut -d' ' -f 8 | cut -d'/' -f 3)

    print_column "${did%@*}" "${did##*@}" "$call_id"
}

print_column "DID" "IP" "Call ID"

for failed_call in $(zgrep "context_2 not found" $TMP_FILE | cut -d ' ' -f 1 | sort | uniq); do
        find_did $failed_call $TMP_FILE
done
