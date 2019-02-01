#! /usr/local/bin/bash

PS=$( ps axwww -o command | grep xfdesktop | grep -v grep )
if [ "${PS}" != "" ]
then
  exit 0
fi

__usage() {
  echo "usage $( basename ${0} ) DIR/FILE"
  exit 1
  }

__absolute() {
  if [ -f /${1} ]
  then
    echo "${1}"
  else
    echo "$( pwd )/${1}"
  fi
  }

[ ${#} -ne 1 ] && __usage
[ -d ${1} -o -f ${1} ] || __usage

[ -f ${1} ] && {
  FILE="${@}"
  WALL="/tmp/$( basename ${0} )_$( basename ${FILE} ).jpg"
}

[ -d ${1} ] && {
  WALLS_LIST=$( find ${1} | egrep "^.*\.[pPjJgG][nNpPiI][gGeEfF][gG]*$" )

  for WALL in ${WALLS_LIST} ;do
    WALLS_COUNT=$(( ${WALLS_COUNT} + 1 ));
  done

  RANDOM=$( head -c 256 /dev/urandom | env LC_ALL=C tr -c -d '1-9' )
  WINNER=$(( ${RANDOM} % ${WALLS_COUNT} + 1 ))
  FILE="$( echo "${WALLS_LIST}" | sed -n ${WINNER}p )"
}

#/bin/cp ~/.fehbg ~/.fehbg.BCK || true
case ${FILE} in
  (*/TILE-*) /usr/local/bin/feh --bg-tile  $( __absolute ${FILE} ) ;;
  (*)        /usr/local/bin/feh --bg-scale $( __absolute ${FILE} ) ;;
esac
