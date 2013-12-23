#!/bin/bash
cd `dirname $0`
sipp -inf users.csv -sf uac_auth_audio.xml -r 1 -d 5000 -s 5000 -i 192.168.5.42 192.168.5.151
