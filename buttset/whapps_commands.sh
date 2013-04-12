#!/bin/bash

fMainMenu() {
        menu=( "List Running Apps:fRunningApps"
                "Stop App:fStopApp" )
        clear
        echo "Whapps Commands"

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

fRunningApps() {
        clear
        /opt/kazoo/utils/sup/sup whapps_controller running_apps
        read -p "Press [Enter] key to continue..."
}

fStopApp() {
        clear
        apps=`/opt/kazoo/utils/sup/sup whapps_controller running_apps`
        apps=${apps/[}
        apps=${apps/]}
        select app in `echo -n $apps | tr "," "\n"`; do
                echo "# /opt/kazoo/utils/sup/sup whapps_controller stop-app $app"
                break
        done
        read -p "Press [Enter] key to continue..."
}

cd `dirname $0`
fMainMenu

