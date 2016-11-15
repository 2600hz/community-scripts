#!/bin/bash

##
##      Backup CouchDB databases
##
##  This script runs couchdb-backup.sh on every database.
##  It then tar/gzips the resulting files and send them to a backup server.
##  If the script fails, it will send an alert E-mail.
##
##  This script requires Daniele Bailo's couchdb-dump
##  https://github.com/danielebailo/couchdb-dump
##

##  Sponsored by GBC Networks Oy (http://gbc.fi)


## read config
echo "Reading config...."
source $( dirname "${BASH_SOURCE[0]}" )/fullbackup-couchdb.conf
TAG=$( basename "${BASH_SOURCE[0]}" )
if [ ! -f $( dirname "${BASH_SOURCE[0]}" )/fullbackup-couchdb.conf ]; then
    logger -t $TAG -s "Error: Can't find config file \"fullbackup-couchdb.conf\""
    exit 6
fi

if [ -z ${couchdb_username+x} ]; then logger -t $TAG -s "Error: CouchDB username is not set in the config file!"; exit 5; fi
if [ -z ${couchdb_password+x} ]; then logger -t $TAG -s "Error: CouchDB password is not set in the config file!"; exit 5; fi
if [ -z ${couchdb_hostname+x} ]; then logger -t $TAG -s "Error: CouchDB hostname/IP is not set in the config file!"; exit 5; fi
if [ -z ${couchdb_port+x} ];     then logger -t $TAG -s "Error: CouchDB port is not set in the config file!"; exit 5; fi
if [ -z ${couchdb_protocol+x} ]; then logger -t $TAG -s "Error: CouchDB transfer protocol is not set in the config file!"; exit 5; fi

if [ -z ${email_enabled+x} ];    then logger -t $TAG -s "Error: Email enabled/disabled state is not set in the config file!"; exit 5; fi
if [ "${email_enabled^^}" != "TRUE" ]; then email_enabled=0; fi
if [ "${email_enabled^^}" == "TRUE" ]; then email_enabled=1; fi
if [ ${email_enabled} -eq 1 ]; then
    if [ -z ${email_hostname+x} ];      then logger -t $TAG -s "Error: Email server hostname/IP is not set in the config file!"; exit 5; fi
fi

if [ -z ${remote_enabled+x} ];   then logger -t $TAG -s "Error: Remote storage enabled/disabled state is not set in the config file!"; exit 5; fi
if [ "${remote_enabled^^}" != "TRUE" ]; then remote_enabled=0; fi
if [ "${remote_enabled^^}" == "TRUE" ]; then remote_enabled=1; fi
if [ ${remote_enabled} -eq 1 ]; then
    if [ -z ${remote_sshkey+x} ];       then logger -t $TAG -s "Error: Remote SSH server key file is not set in the config file!"; exit 5; fi
    if [ -z ${remote_hostname+x} ];     then logger -t $TAG -s "Error: Remote SSH server hostname/IP is not set in the config file!"; exit 5; fi
    if [ -z ${remote_port+x} ];         then logger -t $TAG -s "Error: Remote SSH server port is not set in the config file!"; exit 5; fi
    if [ -z ${remote_protocol+x} ];     then logger -t $TAG -s "Error: Remote SSH server transfer protocol is not set in the config file!"; exit 5; fi
fi

# missing debug parameter is not an error
if [ "${debug^^}" != "TRUE" ]; then debug=0; fi
if [ "${debug^^}" == "TRUE" ]; then debug=1; fi
if [ ${debug} -eq 1 ]; then echo -e "\tDebug mode is enabled"; fi


## make tmp dir
BACK_DATE=`date +%Y-%m-%d `
BACK_DIR=couchbackup_${BACK_DATE}
mkdir -p ${BACK_DIR}


## get databases
echo "Getting CouchDB databases...."
HOST="${couchdb_protocol}://${couchdb_username}:${couchdb_password}@${couchdb_hostname}:${couchdb_port}"
JSON_CONTENT="Content-Type: application/json"
if [ ${debug} -eq 1 ]; then echo -e "\nDEBUG: Executing command = curl -X GET ${HOST}/_all_dbs -H \"${JSON_CONTENT}\"\n"; fi
string=$(curl -X GET ${HOST}/_all_dbs -H "${JSON_CONTENT}" 2>/dev/null)
# remove [ ]
string2=${string#"["}
string2=${string2%"]"}
# split into array
set -f                      # avoid globbing (expansion of *).
array=(${string2//,/ })
set +f


## backup each database
echo -e "Running backup....\n"
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

    # run couchdb-backup.sh
    script_args="-b -u ${couchdb_username} -p ${couchdb_password} -H ${couchdb_protocol}://${couchdb_hostname} -P ${couchdb_port}"
    script_args=${script_args}" -d ${array[i]} -f ${BACK_DIR}/${out_file} "
    exec_cmd=$(dirname "${BASH_SOURCE[0]}")"/couchdb-backup.sh"
    if [ ${debug} -eq 1 ]; then echo -e "\nDEBUG: Executing command = ${exec_cmd} ${script_args}\n"; fi
    unset IFS       # all separators
    result=$( ${exec_cmd} ${script_args} 2>/dev/null )
    error_code=$?

    # check the error code and that the backup file exists and size is non-zero
    if [ ${error_code} -eq 0 ]; then
        if [ -f ${BACK_DIR}/${out_file} ]; then
            if [ -s ${BACK_DIR}/${out_file} ]; then
                # if all is OK, output only the last line of STDOUT
                result="${result##*$'\n'}"
            else
                backup_success=0
                error_message=${error_message}"Error: Backup of ${array[i]} produced a zero byte backup file.\n"
            fi
        else
            backup_success=0
            error_message=${error_message}"Error: Backup of ${array[i]} failed to produce a backup file.\n"
        fi
    else
        backup_success=0
        error_message=${error_message}"Error: Backup of ${array[i]} failed with error code ${error_code}.\n"
        # store first 2 lines of STDOUT for the error_message / alert E-mail
        line_count=0
        IFS=$'\n'       # make newlines the only separator
        for line in $result; do
            error_message=${error_message}"Failure cause: ${line}\n"
            ((line_count++))
            if [ ${line_count} -gt 2 ]; then
                unset IFS       # all separators
                break
            fi
            unset IFS       # all separators
        done
    fi

    # show result of couchbackup execution
    if [ ${debug} -eq 1 ]; then echo -e "DEBUG: couchdb-backup.sh returned = $result\n"; fi

done


## Compress directory
echo "Compressing backup directory...."
COMPRESSED_FILE=couchbackup_${BACK_DATE}.tgz
if [ ${debug} -eq 1 ]; then echo -e "\nDEBUG: Executing command = tar --create --gzip --remove-files --restrict --file ${COMPRESSED_FILE} ${BACK_DIR}\n"; fi
result=$( tar --create --gzip --remove-files --restrict --file ${COMPRESSED_FILE} ${BACK_DIR} 2>&1 )
error_code=$?
if [ ${error_code} -gt 0 ]; then
    backup_success=0
    error_message=${error_message}"Error: Compression of directory ${BACK_DIR} failed with error code ${error_code}.\n"
    # store first 2 lines of STDOUT for the error_message / alert E-mail
    line_count=0
    IFS=$'\n'       # make newlines the only separator
    for line in $result; do
        error_message=${error_message}"Failure cause: ${line}\n"
        ((line_count++))
        if [ ${line_count} -gt 2 ]; then
            break
            unset IFS       # all separators
        fi
    done
    unset IFS       # all separators
fi


## exit 0 if no error
if [ -z ${backup_success+x} ]; then
    backup_success=0;
fi
if [ ${backup_success} -eq 1 ]; then
    echo -e "Backup done!\n"
    exit 0
else
    (>&2 echo -e "\n")
    logger -t $TAG -s "Backup was not successful!";
    (>&2 echo -e "\n")
    if [ ! -z ${error_message+x} ]; then
        (>&2 echo -e ${error_message})
    fi
    exit 1
fi
