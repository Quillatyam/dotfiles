#! /bin/sh

NETFS="nfs,smbfs,fusefs.sshfs"
CMD=doas
DELAY=1

__net_shares_umount() {
  mount -t ${NETFS} -p \
    | awk '{print $2}' \
    | while read MNT
      do
        #DOAS# permit nopass :network as root cmd umount
        #SUDO# %network ALL = NOPASSWD: /sbin/umount -f *
        ${CMD} umount -f "${MNT}"
      done
}

__net_shares_umount

( sleep ${DELAY}; ${CMD} zzz ) &

if pgrep -q mate-screensaver 1> /dev/null 2> /dev/null
then
  mate-screensaver-command --lock 1> /dev/null 2> /dev/null
else
  xlock \
    -mode blank \
    -planfontset '-*-clean-*-*-*-*-*-*-*-*-*-*-iso8859-2' \
    -fontset '-*-clean-*-*-*-*-*-*-*-*-*-*-iso8859-2' \
    -username 'USERNAME: ' \
    -password 'PASSWORD: ' \
    -background gray30 \
    -dpmsoff 1 \
    -message ' ' \
    -info ' '
fi

echo '1' >> ~/.stats/$( basename ${0} )
