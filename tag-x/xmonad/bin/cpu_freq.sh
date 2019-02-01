#!/bin/sh

# CUSTOM MATH FUNCTION
__math() {
  local SCALE=1
  local RESULT=$( echo "scale=${SCALE}; ${@}" | bc -l )
  if echo ${RESULT} | grep --color -q '^\.'
  then
    echo -n 0
  fi
  echo ${RESULT}
  unset SCALE
  unset RESULT
}

FREQ=$(    sysctl -n dev.cpu.0.freq )
TEMP=$(    sysctl -n hw.acpi.thermal.tz0.temperature )
LOAD=$(    sysctl -n vm.loadavg | awk '{print $2}' )
MEM=$(( $( sysctl -n vm.stats.vm.v_inactive_count )
      + $( sysctl -n vm.stats.vm.v_free_count )
      + $( sysctl -n vm.stats.vm.v_cache_count ) ))
MEM=$(     __math ${MEM} \* 4 / 1024 / 1024 )

echo -n "${FREQ}MHz/${TEMP}/${LOAD}/${MEM}GB"
