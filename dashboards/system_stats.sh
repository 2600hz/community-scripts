#!/bin/bash

# Set Graphite host
GRAPHITE=your_influxdb_server_replaceme
GRAPHITE_PORT=2003

send_data ()
{
  # Get epoch timestamp
  DATE=`date +%s`

  # Send data to Graphite + output on the screen
  echo "${DATA} ${DATE}"
  echo "${DATA} ${DATE}" | nc $GRAPHITE $GRAPHITE_PORT
}

# Get ecallmgr data
CALLINFO=`sup -n ecallmgr ecallmgr_maintenance channel_summary |grep @ |awk -F\| '{print $3, $4, $5}'`
SERVERS=`echo "$CALLINFO" | awk '{print $1}' | awk -F@ '{print $2}' | sort | uniq`

for SERVER in $SERVERS; do

        # Count per-server inbound
        SERVER_IN=`echo "$CALLINFO" | grep $SERVER | grep inbound | wc -l`

        # Count per-server outbound
        SERVER_OUT=`echo "$CALLINFO" | grep $SERVER |grep outbound | wc -l`

        REVERSE_SERVER=`echo "$SERVER" | awk -F"." '{for (i=NF;i;i--) printf "%s.",$i; print ""}' | sed 's/.$//'`

	# Send the data out

        DATA="kazoo.calls.inbound.$REVERSE_SERVER.total $SERVER_IN";
        send_data

        DATA="kazoo.calls.outbound.$REVERSE_SERVER.total $SERVER_OUT";
        send_data
done
