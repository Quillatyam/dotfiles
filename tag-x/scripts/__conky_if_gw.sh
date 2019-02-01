#! /bin/sh

GW=$( route -n -4 -v get default 2> /dev/null | awk 'END{print $2}' )

if [ "${GW}" = "0.0.0.0" ]
then
  GW=-
fi

echo -n "${GW}"

echo '1' >> ~/.stats/$( basename ${0} )
