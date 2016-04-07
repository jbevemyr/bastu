#!/bin/bash

if [ "$#" -ne 1 ]; then
    GW=192.168.1.1
else
    GW=$1
fi

ping -c4 $GW > /dev/null

if [ $? != 0 ]
then
  (date >> /var/log/restartlog || true)
  sleep 5
  sudo /sbin/shutdown -r now
fi
