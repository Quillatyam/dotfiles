#! /bin/sh

_usage() {
  echo "usage: $(basename ${0} ) TYPE"
  echo
  echo "type: dzen2 | conky"
  echo
  exit 1
}

# SETTINGS
URI=1.1.1.1

# TYPE
case ${1} in
  (conky|dzen2) :       ;;
  (*)           __usage ;;
esac

# WAIT 2 SECONDS WITH -t OPTION
if ping -c 1 -s 0 -t 2 -q ${URI} 1> /dev/null 2> /dev/null
then
  echo -n OK
else
  COLOR=#dd2200
  case ${1} in
    (conky) printf "%s" "\${color ${COLOR}}NOPE\${color}" ;;
    (dzen2) printf "%s" "^fg(${COLOR})NOPE" ;;
  esac
fi

echo '1' >> ~/.stats/$( basename ${0} )
