#! /bin/sh

OVERHEAD_X=6
OVERHEAD_Y=5
MARGIN_TOP=31
MARGIN_LEFT=1
MARGIN_RIGHT=1

snap() {
  SCREEN_W=$( xdpyinfo | awk '/dimension/ {print $2}' | awk -F 'x' '{print $1}' )
  SCREEN_H=$( xdpyinfo | awk '/dimension/ {print $2}' | awk -F 'x' '{print $2}' )
  case ${1} in
    (L)
      wmctrl -r :ACTIVE: -b remove,maximized_horz
      wmctrl -r :ACTIVE: -e 0,${MARGIN_LEFT},-1,$(( ( ${SCREEN_W} / 2 ) - ${MARGIN_LEFT} - ${OVERHEAD_X} )),-1
      wmctrl -r :ACTIVE: -b add,maximized_vert
      ;;
    (R)
      wmctrl -r :ACTIVE: -b remove,maximized_horz
      wmctrl -r :ACTIVE: -e 0,$(( ( ${SCREEN_W} / 2 ) -1 )),-1,$(( ( ${SCREEN_W} / 2 ) - 3 - ${MARGIN_RIGHT} )),-1
      wmctrl -r :ACTIVE: -b add,maximized_vert
      ;;
    (T)
      X=${MARGIN_LEFT}
      Y=${MARGIN_TOP}
      H=$(( ( ${SCREEN_H} / 2 ) - ${MARGIN_TOP} ))
      wmctrl -r :ACTIVE: -b remove,maximized_vert
      wmctrl -r :ACTIVE: -e 0,${X},${Y},$(( ${SCREEN_W} - ${MARGIN_LEFT} - 4 )),$(( ${H} ))
      ;;
    (B)
      X=${MARGIN_LEFT}
      Y=$(( ( ${SCREEN_H} / 2 ) + ${MARGIN_TOP} ))
      H=$(( ( ${SCREEN_H} / 2 ) - ${MARGIN_TOP} ))
      wmctrl -r :ACTIVE: -b remove,maximized_vert
      wmctrl -r :ACTIVE: -e 0,${X},$(( ${Y} - 8 )),$(( ${SCREEN_W} - ${MARGIN_LEFT} - 4 )),$(( ${H} - 20 ))
      ;;
    (C)
      X=$(( ${SCREEN_W} / 12 ))
      Y=$(( ${SCREEN_H} / 11 ))
      W=$( echo "${SCREEN_W} / 1.20" | bc -l | awk -F '.' '{print $1}' )
      H=$( echo "${SCREEN_H} / 1.25" | bc -l | awk -F '.' '{print $1}' )
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      wmctrl -r :ACTIVE: -e 0,${X},${Y},${W},${H}
      ;;
    (F)
      wmctrl -r :ACTIVE: -b toggle,maximized_vert,maximized_horz
      exit 0
      ;;
    (Q)
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      exit 0
      ;;
    (TL)
    X=${MARGIN_LEFT}
    Y=${MARGIN_TOP}
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      wmctrl -r :ACTIVE: -e 0,${X},${Y},$(( ( ${SCREEN_W} / 2 ) - ${MARGIN_LEFT} - 4 )),$(( ( ${SCREEN_H} / 2 ) - ${MARGIN_TOP} - 3 )),-1
      ;;
    (TR)
    X=$(( ( ${SCREEN_W} / 2 ) - 1 ))
    Y=${MARGIN_TOP}
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      wmctrl -r :ACTIVE: -e 0,${X},${Y},$(( ( ${SCREEN_W} / 2 ) - 3 )),$(( ( ${SCREEN_H} / 2 ) - ${MARGIN_TOP} )),-1
      ;;
    (BL)
    X=${MARGIN_LEFT}
    Y=$(( ( ${SCREEN_H} / 2 ) + ${MARGIN_TOP} - ( ${MARGIN_TOP} / 4 ) ))
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      wmctrl -r :ACTIVE: -e 0,${X},${Y},$(( ${SCREEN_W} / 2 )),$(( ( ${SCREEN_H} / 2 ) - ${MARGIN_TOP} - ( ${MARGIN_TOP} / 2 ) )),-1
      ;;
    (BR)
    X=$(( ( ${SCREEN_W} / 2 ) + ${MARGIN_LEFT} ))
    Y=$(( ( ${SCREEN_H} / 2 ) + ${MARGIN_TOP} - ( ${MARGIN_TOP} / 4 ) ))
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      wmctrl -r :ACTIVE: -e 0,${X},${Y},$(( ${SCREEN_W} / 2 )),$(( ( ${SCREEN_H} / 2 ) - ${MARGIN_TOP} - ( ${MARGIN_TOP} / 2 ) )),-1
      ;;
    (F)
      wmctrl -r :ACTIVE: -b toggle,maximized_vert,maximized_horz
      exit 0
      ;;
    (Q)
      wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
      exit 0
      ;;
    (*)
      echo "usage: ..."
      exit 1
      ;;
  esac

}

# wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
snap ${1}

echo '1' >> ~/.stats/$( basename ${0} )

