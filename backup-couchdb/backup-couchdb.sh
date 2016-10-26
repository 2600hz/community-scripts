#!/bin/bash

##
## Backup CouchDB databases
##

## internal functions
function setval { printf -v "$1" "%s" "$(cat)"; declare -p "$1"; }

## read config
echo "Reading config...." >&2
source $( dirname "${BASH_SOURCE[0]}" )/backup-couchdb.conf
if [ -z ${username+x} ]; then echo "Error: Username is not set in the config file!"; exit 5; fi
if [ -z ${password+x} ]; then echo "Error: Password is not set in the config file!"; exit 5; fi
if [ -z ${hostname+x} ]; then echo "Error: hostname is not set in the config file!"; exit 5; fi
if [ -z ${port+x} ];     then echo "Error: Port is not set in the config file!"; exit 5; fi
if [ -z ${protocol+x} ];     then echo "Error: Transfer protocol is not set in the config file!"; exit 5; fi

## make dir
BACK_DATE=`date +%Y-%m-%d `
BACK_DIR=couchbackup_${BACK_DATE}
mkdir -p ${BACK_DIR}

## get databases
HOST="${protocol}://${username}:${password}@${hostname}:${port}"
JSON_CONTENT="Content-Type: application/json"
echo -e "\tGet Databases"
string=$(curl -X GET ${HOST}/_all_dbs -H "${JSON_CONTENT}")

# remove [ ]
string2=${string#"["}
string2=${string2%"]"}

# split into array
set -f                      # avoid globbing (expansion of *).
array=(${string2//,/ })
set +f

## set environment variables for CouchBackup
export COUCH_URL=${HOST}
export DEBUG=couchbackup

## backup each database
echo -e "\n"
for i in "${!array[@]}"
do
    # initialize success flag only on first iteration
    if [ $i -eq 0 ]; then
        backup_success=1
    fi

    # quicky URL-encode
    array[i]=$(echo ${array[i]} | sed -e "s/\//%2F/g")
    array[i]=$(echo ${array[i]} | sed -e "s/\+/%2B/g")

    # remove " "
    array[i]=${array[i]%"\""}
    array[i]=${array[i]#"\""}

    out_file=couchdb-${array[i]}_${BACK_DATE}.txt
    echo -e "Backup #$i \t${out_file}"

    # run couchbackup
    eval "$( couchbackup --db ${array[i]} 2> >(setval errval) > ${BACK_DIR}/${out_file}; )"

    # check that backup file exists and size is non-zero
    if [ -f ${BACK_DIR}/${out_file} ]; then
        if [ -s ${BACK_DIR}/${out_file} ]; then
            # if all is OK, output only the last line of STDERR
            result="${errval##*$'\n'}"
        else
            backup_success=0
            error_message=${error_message}"Error: Backup of ${array[i]} produced a zero byte backup file.\n"
            result="${errval}"
        fi
    else
        backup_success=0
        error_message=${error_message}"Error: Backup of ${array[i]} failed to produce a backup file.\n"
        result="${errval}"
    fi

    # show result of couchbackup execution
    echo -e "OUTPUT = $result\n"

done
if [ -z ${backup_success+x} ]; then
    backup_success=0;
fi

## exit 0 if no error
if [ ${backup_success} -eq 1 ]; then
    exit 0
else
    echo "Backup was not successful!";
    if [ ! -z ${error_message+x} ]; then
        echo -e ${error_message}
    fi
    exit 1
fi

