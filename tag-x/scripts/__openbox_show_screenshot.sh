#! /bin/sh

viewnior "$HOME/screenshots/$( ls -tr $HOME/screenshots|tail -1 )"

echo '1' >> ~/.stats/$( basename ${0} )
