ping -c4 192.168.1.1 > /dev/null

if [ $? != 0 ]
then
  (date >> /var/log/restartlog || true)
  sleep 5
  sudo /sbin/shutdown -r now
fi
