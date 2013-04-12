#!/bin/bash

fMainMenu() {
        menu=( "Registrations per minute:fRegPerMin"
                "Calls per minute:fCallsPerMin" )
	clear
	echo "Log File Anaylzer"

        PS3="Type a number or 'q' to return: "
        select selection in "${menu[@]%%:*}"; do
                for idx in ${!menu[@]}; do
                        if [ "${menu[$idx]%%:*}" = "$selection" ]; then
                                ${menu[$idx]#*:}     
                        fi
                done
                break
        done
}

fRegPerMin() {
	clear
	grep "successful registration" /var/log/2600hz-platform.log | cut -d' ' -f3 | uniq -c | awk '{if(min==""){min=max=$1}; if($1>max) {max=$1}; if($1< min) {min=$1}; total+=$1; count+=1} END {print total/count, min, max}'
	read -p "Press [Enter] key to continue..."
}

fCallsPerMin() {
	clear
	grep "received a request asking if callflows" /var/log/2600hz-platform.log | cut -d' ' -f3 | uniq -c | awk '{if(min==""){min=max=$1}; if($1>max) {max=$1}; if($1< min) {min=$1}; total+=$1; count+=1} END {print total/count, min, max}'
	read -p "Press [Enter] key to continue..."
}

cd `dirname $0`
fMainMenu
