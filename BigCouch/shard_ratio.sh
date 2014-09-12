#!/bin/bash

for DB in `find /srv/db/shards/ -size +$1 -exec ls -l {} \; | awk '{ print $5, $9 }' | sort -rn | cut -d' ' -f2 | sed -r 's|^.{33}||;s|\..*$||' | uniq`
do
  JSON=`curl -s 127.0.0.1:5984/${DB//\//%2F}`
  DATASIZE=`echo $JSON | grep -Eo '"data_size":([0-9]+)' | cut -d':' -f2`
  DISKSIZE=`echo $JSON | grep -Eo '"disk_size":([0-9]+)' | cut -d':' -f2`
  RATIO=`bc -l <<< "($DISKSIZE / $DATASIZE) * 100"`
  RATIO=${RATIO%.*}
  if [ "${RATIO}" -gt "150" ];
  then
    echo ${RATIO} ${DB}
  fi
done | sort -rn
