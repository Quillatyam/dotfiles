#! /bin/sh

#### XRANDR=$( xrandr )
#### COUNT=$( echo "${XRANDR}" | grep -c " connected " )
#### DELAY=0.2
PROFILE=T420s

killall -9 conky

nice -n 20 conky -c /home/robert/.conkyrc &

#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.1 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.2 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.3 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.4 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.5 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.6 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.7 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.8 1> /dev/null 2> /dev/null &
#nice -n 20 conky -c /home/robert/.conkyrc.1.9.${PROFILE}.LOG.9 1> /dev/null 2> /dev/null &

echo '1' >> ~/.stats/$( basename ${0} )
