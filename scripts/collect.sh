#!/bin/bash

SERVERS=(1 2)
FILENAME="hping_log.csv"

for NAME in ${SERVERS[*]}
do
	USER="azureuser"
	ADDR="coracle-"$NAME".cloudapp.net"
	scp $USER@$ADDR:$FILENAME ~/$NAME"-results.csv" 
done