#!/bin/bash

if [ "$#" -ne 1 ]; then
    GW=192.168.1.1
else
    GW=$1
fi

ping -c4 $GW > /dev/null

if [ $? != 0 ]
then
  sudo sh -c 'date >> /var/log/restartlog'
  sleep 5
  sudo /sbin/shutdown -r now
fi
