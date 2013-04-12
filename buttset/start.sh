#!/bin/bash

fMainMenu() {
        menu=( "System Status:fSystemStatus"
		"Analyze log file:fAnalyzeMenu"
                "Ecallmgr Commands:fEcallmgr"
                "Whapps Commands:fWhapps" )
	clear
	echo "Main Menu"
        PS3="Type a number or 'q' to quit: "
    
        select selection in "${menu[@]%%:*}"; do
		for idx in ${!menu[@]}; do
			if [ "${menu[$idx]%%:*}" = "$selection" ]; then
				${menu[$idx]#*:} 
			fi	
		done
                break
        done
}

fSystemStatus() {
	./system_status.sh
}

fAnalyzeMenu() {
        ./log_analyzer.sh
}

fEcallmgr() {
        ./ecallmgr_commands.sh
}

fWhapps() {
        ./whapps_commands.sh
}

cd `dirname $0`

export KAZOO_SUP="/opt/kazoo/utils/sup/sup"

fMainMenu
