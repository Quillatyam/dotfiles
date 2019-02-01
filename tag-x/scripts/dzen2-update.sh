#! /bin/sh

# SETTINGS
CLA='^fg(#666666)'
CVA='^fg(#eeeeee)'
CDE='^fg(#000000)'

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

# GATHER DATA
DATE=$(    date +'%a, %d %b  %H:%M' )
FREQ=$(    sysctl -n dev.cpu.0.freq )
TEMP=$(    sysctl -n hw.acpi.thermal.tz0.temperature )
LOAD=$(    sysctl -n vm.loadavg | awk '{print $2}' )
MEM=$(( $( sysctl -n vm.stats.vm.v_inactive_count )
      + $( sysctl -n vm.stats.vm.v_free_count )
      + $( sysctl -n vm.stats.vm.v_cache_count ) ))
MEM=$(     __math ${MEM} \* 4 / 1024 / 1024 )
IF_IP=$(   ~/.scripts/__conky_if_ip.sh )
IF_GW=$(   ~/.scripts/__conky_if_gw.sh )
IF_DNS=$(  ~/.scripts/__conky_if_dns.sh )
IF_PING=$( ~/.scripts/__conky_if_ping.sh dzen2 )
VOL=$(     mixer -s vol | awk -F ':' '{printf("%s",$2)}' )
PCM=$(     mixer -s pcm | awk -F ':' '{printf("%s",$2)}' )
FS=$(      zfs list -H -d 0 -o name,avail | awk '{printf("%s/%s ",$1,$2)}' )
BAT=$(     ~/.scripts/__conky_battery.sh dzen2 )
PS=$(      ps ax -o %cpu,rss,comm | sed 1d | bsdgrep -v 'idle$' | sort -r -n \
             | head -3 | awk '{printf("%s/%d%%/%.1fGB ",$3,$1,$2/1024/1024)}' )

# PRESENT DATA
echo -n        " ${CLA}DATE: ${CVA}${DATE} "
echo -n "${CDE}| ${CLA}SYS: ${CVA}${FREQ}MHz/${TEMP}/${LOAD}/${MEM}GB "
echo -n "${CDE}| ${CLA}IP: ${CVA}${IF_IP}"            # NO SPACE AT THE END
echo -n "${CDE}| ${CLA}GW: ${CVA}${IF_GW} "
echo -n "${CDE}| ${CLA}DNS: ${CVA}${IF_DNS} "
echo -n "${CDE}| ${CLA}PING: ${CVA}${IF_PING} "
echo -n "${CDE}| ${CLA}VOL: ${CVA}${VOL}/${PCM} "
echo -n "${CDE}| ${CLA}FS: ${CVA}${FS}"               # NO SPACE AT THE END
echo -n "${CDE}| ${CLA}BAT: ${CVA}${BAT} "
echo -n "${CDE}| ${CLA}TOP: ${CVA}${PS}"              # NO SPACE AT THE END
echo

echo '1' >> ~/.stats/$( basename ${0} )
