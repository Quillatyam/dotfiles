#! /bin/sh

# XRANDR=$( xrandr )
# COUNT=$( echo "${XRANDR}" | grep -c " connected " )
# PROFILE=T420s

killall -9 dzen2

~/.scripts/dzen2-fifo.sh &

echo '1' >> ~/.stats/$( basename ${0} )
