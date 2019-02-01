#! /bin/sh

# SETTINGS
COLOR_WHITE=#ffffff
COLOR_ORANGE=#ffaa00
COLOR_RED=#dd2200

__color_time() { # 1=TIME
  local TIME=${1}
  if [ ${TIME} -gt 90 ]
  then
    COLOR_TIME=${COLOR_WHITE}
  elif [ ${TIME} -lt 90 -a ${TIME} -gt 30 ]
  then
    COLOR_TIME=${COLOR_ORANGE}
  elif  [ ${TIME} -lt 30 ]
  then
    COLOR_TIME=${COLOR_RED}
  fi
}

__color_life() { # 1=LIFE
  local LIFE=${1}
  if [ ${LIFE} -gt 50 ]
  then
    COLOR_LIFE=${COLOR_WHITE}
  elif [ ${LIFE} -lt 50 -a ${LIFE} -gt 25 ]
  then
    COLOR_LIFE=${COLOR_ORANGE}
  elif  [ ${LIFE} -lt 25 ]
  then
    COLOR_LIFE=${COLOR_RED}
  fi
}

__usage() {
  echo "usage: $(basename ${0} ) TYPE"
  echo
  echo "type: dzen2 | conky"
  echo
  exit 1
}

# TYPE
case ${1} in
  (conky|dzen2) :       ;;
  (*)           __usage ;;
esac

LIFE=$( sysctl -n hw.acpi.battery.life )
case $( sysctl -n hw.acpi.acline ) in
  (1)
    __color_life ${LIFE}
    case ${1} in
      (conky) echo "AC/\${color ${COLOR_LIFE}}${LIFE}%\${color}" ;;
      (dzen2) echo "AC/^fg(${COLOR_LIFE})${LIFE}%" ;;
    esac
    ;;
  (0)
    TIME=$( sysctl -n hw.acpi.battery.time )
    if [ "${TIME}" != "-1" ]
    then
      HOUR=$(( ${TIME} / 60 ))
      MINS=$(( ${TIME} % 60 ))
      [ ${MINS} -lt 10 ] && MINS="0${MINS}"
    else
      # WE HAVE TO ASSUME SOMETHING SO LETS ASSUME 2:22
      TIME=142
      HOUR=2
      MINS=22
    fi
    __color_time ${TIME}
    __color_life ${LIFE}
    case ${1} in
      (conky) echo "\${color ${COLOR_TIME}}${HOUR}:${MINS}\${color}/${LIFE}%" ;;
      (dzen2) echo "^fg(${COLOR_TIME})${HOUR}:${MINS}^fg()/${LIFE}%"          ;;
    esac
    ;;
esac

echo '1' >> ~/.stats/$( basename ${0} )
