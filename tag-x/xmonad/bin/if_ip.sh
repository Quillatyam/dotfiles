#! /bin/sh

# SETTINGS
WLAN=wlan0
WWAN=tun0
WWAN_FAIL=0

# WWAN DEBUG
# if [ ! -e /dev/cuaU0 ]
# then
#   echo -n "${WWAN}/FAIL "
#   WWAN_FAIL=1
# fi

case $( ifconfig -u | grep -v '127.0.0.1' | grep -c 'inet ' ) in
  (0)
    if [ ${WWAN_FAIL} -ne 1 ]
    then
      echo -n '- '
      exit 0
    fi
    ;;
  (*)
    for I in $( ifconfig -l -u | sed s/lo0//g )
    do
      if [ "${I}" = "${WWAN}" ]
      then
        if [ ${WWAN_FAIL} -eq 1 ]
        then
          continue
        fi
      fi
      IFCONFIG=$( ifconfig ${I} )
      if [ "${I}" = "${WLAN}" ]
      then
        echo -n ${I}
        SSID=$( echo "${IFCONFIG}" | grep 'ssid' )
        if echo "${SSID}" | grep -q '"'
        then
          SSID=$( echo "${IFCONFIG}" | awk -F \" '/ssid/ {print $2}' )
        else
          SSID=$( echo "${IFCONFIG}" | awk '/ssid/ {print $2}' )
        fi
        if [ "${SSID}" != "" ]
        then
          echo -n "/${SSID}"
        else
          echo -n "/-"
        fi
        SSID=''
        INET=$( echo "${IFCONFIG}" | awk '/inet / {print $2}' )
        echo "${INET}" \
          | while read IP
            do
              echo -n "/${IP}"
            done
        echo -n ' '
        INET=''
      else
        INET=$( echo "${IFCONFIG}" | awk '/inet / { printf("/%s",$2) }' )
        if [ "${INET}" != "" ]
        then
          echo -n "${I}${INET} "
        fi
        INET=''
      fi
    done
    ;;
esac
