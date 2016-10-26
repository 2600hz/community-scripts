#!/bin/bash

##
##      Backup CouchDB databases
##
##  This script runs couchdb-backup.sh on every database.
##  It then tar/gzips the resulting files and send them to a backup server.
##  If the script fails, it will send an alert email.
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
    if [ -z ${email_address+x} ];      then logger -t $TAG -s "Error: Email address for alert email is not set in the config file!"; exit 5; fi
fi

if [ -z ${remote_enabled+x} ];   then logger -t $TAG -s "Error: Remote storage enabled/disabled state is not set in the config file!"; exit 5; fi
if [ "${remote_enabled^^}" != "TRUE" ]; then remote_enabled=0; fi
if [ "${remote_enabled^^}" == "TRUE" ]; then remote_enabled=1; fi
if [ ${remote_enabled} -eq 1 ]; then
    if [ -z ${remote_sshkey+x} ];       then logger -t $TAG -s "Error: Remote SSH server key file is not set in the config file!"; exit 5; fi
    if [ -z ${remote_username+x} ];     then logger -t $TAG -s "Error: Remote SSH server username is not set in the config file!"; exit 5; fi
    if [ -z ${remote_hostname+x} ];     then logger -t $TAG -s "Error: Remote SSH server hostname/IP is not set in the config file!"; exit 5; fi
    if [ -z ${remote_port+x} ];         then logger -t $TAG -s "Error: Remote SSH server port is not set in the config file!"; exit 5; fi
    if [ -z ${remote_protocol+x} ];     then logger -t $TAG -s "Error: Remote SSH server transfer protocol is not set in the config file!"; exit 5; fi
    if [ -z ${remote_directory+x} ];     then logger -t $TAG -s "Error: Remote SSH server directory is not set in the config file!"; exit 5; fi
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
echo -e "Getting CouchDB databases....\n"
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
echo -e "Taking backups....\n"
for i in "${!array[@]}"
do
    # initialize success flag only on first iteration
    if [ $i -eq 0 ]; then
        backup_success=1
    fi

    db_name=${array[i]}

    # URL-encode
    db_name=$(echo ${db_name} | sed -e "s/\//%2F/g")
    db_name=$(echo ${db_name} | sed -e "s/\+/%2B/g")

    # remove " "
    db_name=${db_name%"\""}
    db_name=${db_name#"\""}

    out_file=couchdb-${db_name}_${BACK_DATE}.txt
    echo -e "Backup #$i \t${out_file}"

    # run couchdb-backup.sh
    script_args="-b -u ${couchdb_username} -p ${couchdb_password} -H ${couchdb_protocol}://${couchdb_hostname} -P ${couchdb_port}"
    script_args=${script_args}" -d ${db_name} -f ${BACK_DIR}/${out_file} "
    exec_cmd=$(dirname "${BASH_SOURCE[0]}")"/couchdb-backup.sh"
    if [ ${debug} -eq 1 ]; then echo -e "\nDEBUG: Executing command = ${exec_cmd} ${script_args}\n"; fi
    unset IFS       # all separators
    result=$( ${exec_cmd} ${script_args} 2>/dev/null )
    error_code=$?

    # check the error code, that the backup file exists and size is non-zero
    if [ ${error_code} -eq 0 ]; then
        if [ -f ${BACK_DIR}/${out_file} ]; then
            if [ ! -s ${BACK_DIR}/${out_file} ]; then
                backup_success=0
                error_message=${error_message}"Error: Backup of ${db_name} produced a zero byte backup file.\n"
            fi
        else
            backup_success=0
            error_message=${error_message}"Error: Backup of ${db_name} failed to produce a backup file.\n"
        fi
    else
        # skip errors for _metadata and _replicator databases
        if [ ${db_name} == '_metadata' ]; then
                if [ ${debug} -eq 1 ]; then echo -e "DEBUG: skipping errors for '_metadata' database\n"; fi
                continue
        elif [ ${db_name} == '_replicator' ]; then
                if [ ${debug} -eq 1 ]; then echo -e "DEBUG: skipping errors for '_replicator' database\n"; fi
                continue
        fi
        backup_success=0
        error_message=${error_message}"Error: Backup of ${db_name} failed with error code ${error_code}.\n"
        # store first 2 lines of STDOUT for the error_message / alert email
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
    # store first 2 lines of STDOUT for the error_message / alert email
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


## Move to SSH server
echo "Moving backup to remote server...."
if [ ${remote_enabled} -eq 1 ]; then
    unset IFS       # all separators
    if [ "${remote_protocol^^}" == "SCP" ]; then
        exec_cmd="scp -p -i ${remote_sshkey} -P ${remote_port} ${COMPRESSED_FILE} ${remote_username}@${remote_hostname}:${remote_directory}/ "
        if [ ${debug} -eq 1 ]; then echo -e "\nDEBUG: Executing command = ${exec_cmd}\n"; fi
        result=$( ${exec_cmd} 2>&1 )
        error_code=$?
    elif [ "${remote_protocol^^}" == "SFTP" ]; then
        exec_cmd="sftp -i ${remote_sshkey} -P ${remote_port} ${remote_username}@${remote_hostname}"
        if [ ${debug} -eq 1 ]; then echo -e "\nDEBUG: Executing command = ( printf \"cd ${remote_directory} \\\nput couchbackup_2016-11-21.tgz \\\nquit\" ) | ${exec_cmd}\n"; fi
        result=$( ( printf "cd ${remote_directory} \nput couchbackup_2016-11-21.tgz \nquit" ) | ${exec_cmd} 2>&1 )
        error_code=$?
    else
        error_code=0
        backup_success=0
        logger -t $TAG -s "Error: Unknown Remote SSH server transfer protocol \" ${remote_protocol}\" "
    fi

    # check the error code and delete file if everything is OK
    if [ ${error_code} -eq 0 ]; then
        rm -f ${COMPRESSED_FILE}
    else
        backup_success=0
        error_message=${error_message}"Error: ${remote_protocol} command failed with error code ${error_code}.\n"
        # store first 2 lines of STDOUT for the error_message / alert email
        line_count=0
        IFS=$'\n'       # make newlines the only separator
        for line in $result; do
            error_message=${error_message}"Failure cause: ${line}\n"
            ((line_count++))
            if [ ${line_count} -gt 2 ]; then
                unset IFS       # all separators
                break
            fi
        done
        unset IFS       # all separators
    fi

fi


## exit 0 if no error
if [ -z ${backup_success+x} ]; then
    backup_success=0;
fi
if [ ${backup_success} -eq 1 ]; then
    echo -e "Backup done!\n"
    exit 0
else
    # show error messages
    (>&2 echo -e "\n")
    logger -t $TAG -s "Backup was not successful!"
    (>&2 echo -e "\n")
    if [ ! -z ${error_message+x} ]; then
        IFS=$'\n'       # make newlines the only separator
        for line in $(echo -e ${error_message}); do
            logger -t $TAG -s ${line}
        done
        unset IFS       # all separators
    fi
    # send alert email
    if [ ${email_enabled} -eq 1 ]; then
        header="-a From:fullbackup-couchdb@$( hostname )"
        error_message="Backup was not successful!\nBackup host $( hostname )\n\n\tError messages:\n"${error_message}
        # replace password with XXXXXXXXXX
        error_message=${error_message//${couchdb_password}/XXXXXXXXXX}
        echo -e ${error_message} | mailx ${header} -s "Alert: Backup of CouchDB failed!" ${email_address}
    fi
    exit 1
fi
