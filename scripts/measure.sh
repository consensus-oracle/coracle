#!/bin/bash

SERVERS=(1 2)

LOG_FILE="hping_log.csv"
touch $LOG_FILE

for i in `seq 1 10`
do
	for NAME in ${SERVERS[*]}
	do
		SNAME="coracle-"$NAME".cloudapp.net"
		echo $SNAME
		hping3 $SNAME -c 3 -p 80 -S 2> output.temp
		RTT=`cat output.temp | grep round-trip | awk -F '/' '{print $4}'`
		TIME=`date`
		echo "$TIME, $NAME, $RTT" >> $LOG_FILE 
	done
done