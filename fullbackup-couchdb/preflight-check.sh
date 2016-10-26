#!/bin/bash

##
##      Check software dependencies for fullbackup-couchdb.sh
##
##  This script check that the required software is found on the system.
##  You only need to run this script once during setup.
##

##  Sponsored by GBC Networks Oy (http://gbc.fi)


declare -a gnu_programs=(basename bash cat date dirname echo hostname mkdir printf rm source)
declare -a other_programs=(logger mailx scp sftp ./couchdb-backup.sh)

RED='\033[0;31m'
NC='\033[0m' # No Color

##START FUNCTION
check_file(){
    program=$1

    if (type $program >/dev/null 2>&1); then
        echo -e "${NC}\t$program found"
    else
        (>&2 echo -e "${RED}    Error: $program is required but it's not installed!${NC}")
#        exit 5;
    fi
}
## END FUNCTION


echo "Checking standard GNU software..."
for program in "${gnu_programs[@]}"; do
    check_file $program
done
echo "Checking other programs..."
for program in "${other_programs[@]}"; do
    check_file $program
done
echo "Check completed."
