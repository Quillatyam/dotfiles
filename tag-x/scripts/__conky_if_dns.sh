#! /bin/sh

# ONLY FIRST DNS FROM /etc/resolv.conf FILE
NS=$( awk '/^nameserver/ {print $2; exit}' /etc/resolv.conf )
if [ "${NS}" != "" ]
then
  echo -n ${NS}
else
  echo -n -
fi

echo '1' >> ~/.stats/$( basename ${0} )
