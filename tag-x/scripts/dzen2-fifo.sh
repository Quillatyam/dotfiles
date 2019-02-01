#! /bin/sh
# SETTINGS
DELAY=60
FONT='IBMPlexMono-9'
FG='#eeeeee'
BG='#111111'
NR="(sh|tail)\ .*dzen2-fifo"
ME=$$
# DOS='onstart=raise'
DOS='onstart=lower'
DB1='button1=exec:~/.scripts/dzen2-update.sh > ~/.dzen2-fifo'
PS=$( ps axwww -o pid,command )

# KILL ALL ALIVE dzen2(1) PROCESSES
if echo "${PS}" | grep -q -E "${NR}" 2> /dev/null
then
  PIDS=$( echo "${PS}" | grep -E "${NR}" | awk '{print $1}' | grep -v ${ME} | tr '\n' ' ' )
  if [ ! -z "${PIDS}" ]
  then
    kill -9 ${PIDS}
  fi
fi

# CLEAN OLD dzen2(1) FIFO
rm -f ~/.dzen2-fifo

# MAKE NEW dzen2(1) FIFO
mkfifo ~/.dzen2-fifo

# START dzen2(1) TO UPDATE FIFO
while true
do
  tail -1 ~/.dzen2-fifo
done | dzen2 \
      -w 3600 \
      -fg "${FG}" \
      -bg "${BG}" \
      -fn "${FONT}" \
      -ta l \
      -e "${DOS};${DB1}" &

# RUN FITST dzen2(1) UPDATE
~/.scripts/dzen2-update.sh > ~/.dzen2-fifo &

echo '1' >> ~/.stats/$( basename ${0} )
