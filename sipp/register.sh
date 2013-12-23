#!/bin/bash
cd `dirname $0`
sipp -inf users.csv -sf register.xml -d 1000 -s 5000 -i 192.168.5.42 192.168.5.151
