ping -c4 192.168.1.1 > /dev/null
 
if [ $? != 0 ] 
then
  sudo /sbin/shutdown -r now
fi
