#! /bin/sh

PS=$( ps axwww -o pid,command )
for PID in /usr/local/libexec/gconfd-2 \
           /usr/local/libexec/gvfs-hal-volume-monitor \
           /usr/local/libexec/gvfs-gphoto2-volume-monitor \
           /usr/local/libexec/gvfsd \
           /usr/local/libexec/gvfsd-metadata \
           /usr/local/libexec/gvfsd-fuse \
           /usr/local/lib/tumbler-1/tumblerd \
           /usr/local/lib/xfce4/xfconf/xfconfd
do
  PIDS=$( echo "${PS}" | grep ${PID} | awk '{print $1}' | tr '\n' ' ' )
  if [ ! -z "${PIDS}" ]
  then
    kill -9 ${PIDS}
  fi
done

echo '1' >> ~/.stats/$( basename ${0} )
