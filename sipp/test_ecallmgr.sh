#!/bin/bash

echo -n "Starting ecallmgr..."

/opt/whistle/whistle/ecallmgr/start-dev.sh &

echo "done"

sleep 6

echo -n "Stopping ecallmgr..."

kill %1

echo "done"

./$0 &
