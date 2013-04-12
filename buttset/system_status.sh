#!/bin/bash

fMainMenu() {
        menu=( "Connected FS servers:fConnectedServers"
                "Configured FS Servers:fListServers" )
	clear
	echo "Ecallmgr Commands"

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

fConnectedServers() {
	clear
	sup -necallmgr ecallmgr_manintenance list_fs_nodes
	read -p "Press [Enter] key to continue..."
}

fListServers() {
	clear
	sup -necallmgr ecallmgr_config get fs_nodes
	read -p "Press [Enter] key to continue..."
}

cd `dirname $0`

echo $KAZOO_SUP
