#!/bin/bash
 
. /etc/init.d/functions
 
declare -A ERL_PIDS
 
CORES=`grep -E "^processor" /proc/cpuinfo |wc -l`
if [ "$CORES" = "1" ]; then
  BEAM=beam
else
  BEAM=beam.smp
fi
 
check_proc ()
{
  proc=$1
  name=$2
  rpm_name=$3
  rpm_check=$(check_rpm $3)
  if [[ $rpm_check == "1" ]]
  then warning
    echo "$2 is not installed"
  else
    proc_status=`/etc/init.d/$proc status 2> /dev/null`
    if [ ! -f /var/run/$proc.pid ]
    then
      if [ -f /var/run/$proc/$proc.pid ]
      then
        proc_pid=`ps -p \`cat /var/run/$proc/$proc.pid 2> /dev/null\` 2> /dev/null`
      fi
    else
      proc_pid=`ps -p \`cat /var/run/$proc.pid 2> /dev/null\` 2> /dev/null`
    fi
 
    if [[ $proc_pid == *"$proc"* && $proc_status == *"is running"* ]]
    then success
      echo "$name is running"
    elif [[ $proc_pid == *"$proc"* || $proc_status == *"is running"* ]]
    then warning
      echo "$name might not be running properly"
    else failure
      echo "$name is not running"
    fi
    echo "  Version: $rpm_check"
  fi
}
 
check_erl_proc()
{
  proc=$1
  name=$2
  rpm_name=$3
  rpm_check=$(check_rpm $3)
  if [[ $rpm_check == "1" ]]
  then warning
    echo "$2 is not installed"
  else
    for i in `pidof $BEAM`; do
      if cat /proc/$i/cmdline | grep -Eq "name[^\-]+$proc"; then
        ERL_PIDS[$proc]=$i
      fi
    done
 
    if [[ `ps -p ${ERL_PIDS[$proc]} 2> /dev/null` == *"beam"* ]]
    then success
      echo "$name is running"
    else failure
      echo "$name is not running"
    fi
    echo "  Version: $rpm_check"
  fi
}
 
check_rpm()
{
  RPM=`rpm -qa | grep $1`
  if [[ $RPM ]]
  then
    echo "$RPM"
  else
    echo "1"
  fi
}
 
check_erl_proc rabbit Kazoo-RabbitMQ kazoo-R1[5-6]
check_proc kamailio Kazoo-Kamailio kazoo-kamailio
check_erl_proc whistle_apps Whistle-Apps kazoo-R1[5-6]
check_erl_proc ecallmgr Ecallmgr kazoo-R1[5-6]
check_proc httpd HTTPd httpd-[0-9]
check_proc haproxy HAProxy haproxy-[0-9]
check_proc freeswitch Kazoo-FreeSWITCH kazoo-freeswitch-R1[5-6]
check_erl_proc bigcouch Kazoo-Bigcouch kazoo-bigcouch-R1[5-6]
