#!/bin/bash
# A useful tool for viewing FreeSWITCH SIP logs
# 2600hz - The Future of Cloud Telecom
#
# Usage: ./sipify.sh /var/log/freeswitch/debug.log | grep your_call_id
# If the log is gzipped:
# zcat /var/log/freeswitch/debug.log.2.gz | ./sipify.sh | grep your_call_id

awk 'BEGIN{I=0};/^(send|recv).*$/{SIPPACKET=1}; SIPPACKET==1{PACKET[I++]=$0;}; SIPPACKET==1&&/^\ +(Call-ID:|i:).*$/{CALLID=$2}; SIPPACKET==0{print $0}; /^\ +-+$/{PRINT=(SIPPACKET==1?(PRINT==1?0:1):0);SIPPACKET=(PRINT==1?1:0);if(SIPPACKET==0){for(i=0;i<I;i++){print CALLID, PACKET[i]; delete PACKET[i];}I=0;CALLID="";}};' "$@" <&0

