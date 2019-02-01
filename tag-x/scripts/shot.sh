#! /bin/sh
DATE=$( date +%Y-%m-%d_%H-%M-%S )
DIR=~/screenshots
FILE=${DIR}/${USER}_${DATE}.png

mkdir -p ${DIR} || {
  echo "ER: cannot create ${DIR} dir"
  exit 1
}

case ${#} in
  (1) [ ${1} = "-s" ] && OPTS="-s" ;;
esac

scrot ${OPTS} ${FILE}

echo '1' >> ~/.stats/$( basename ${0} )
