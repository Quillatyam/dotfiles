#! /bin/sh

DELAY=0.1

killall -9 tint2

( sleep ${DELAY} &&            tint2 -c ~/.tint2rc         1> /dev/null 2> /dev/null ) &

echo '1' >> ~/.stats/$( basename ${0} )
