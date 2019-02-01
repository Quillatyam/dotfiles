#/bin/sh

#THEME=$( ls ~/.config/Xdefaults | sort -R | head -1 )
#TITLE=$( echo ${THEME} | awk -F. '{print $3}' )

xrdb -load  ~/.Xdefaults
xrdb -merge ~/.config/Xdefaults/Xdefaults.theme.3024NIGHT

xterm -title "xterm" ${@} &

echo '1' >> ~/.stats/$( basename ${0} )
