#!/bin/bash

## Usage: ./context_2_failures.bash /var/log/freeswitch/kazoo_debug.log
##                   Datetime |            DID |                   IP | Call-ID
## 2017-05-18_15:44:34.564716 |     1234567890 |     10.10.10.10:5060 | abc123@10.10.10.10
## 2017-05-18_15:44:35.564716 |    +1345678901 |     20.20.20.20:5060 | 123789128371298@20.20.20.20
## 2017-05-18_15:44:36.564716 |    +1456789012 |     30.30.30.30:5060 | 291834576asdfk@30.30.30.30
##
## Works with glob patterns too
## ./context_2_failures.bash /var/log/freeswitch/kazoo_debug.log.3?.gz

#!/bin/bash

function print_column {
    printf "%28s | %15s | %20s | %s\n" $1 $2 $3 $4
}

function find_did {
    local call_id=$1
    local file=$2

    local log_line=$(zgrep "^$call_id .* New Channel sofia/sipinterface_1" $file)
    local did=$(echo $log_line | cut -d' ' -f 8 | cut -d'/' -f 3)
    local datetime=$(echo $log_line | cut -d' ' -f 2,3)

    print_column "${datetime/ /_}" "${did%@*}" "${did##*@}" "$call_id"
}

function search_log_file {
    md5_hash=$(md5sum $1 | cut -d ' ' -f 1)
    tmp_file="/tmp/$md5_hash.${1##*.}"

    if [ ! -f $tmp_file ]; then
        cp $1 $tmp_file
    fi

    for failed_call in $(zgrep "context_2 not found" $tmp_file | cut -d ' ' -f 1 | sort | uniq); do
        find_did $failed_call $tmp_file
    done
}

print_column "Datetime" "DID" "IP" "Call-ID"
for log_file in $@; do
    search_log_file $log_file
done
