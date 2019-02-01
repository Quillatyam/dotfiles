#! /bin/sh

FILE=$( awk -F\' '/feh/ {print $(NF-1)}' ~/.fehbg )

# echo "${FILE}" > /tmp/$( basename ${0} ).out

echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
echo "<openbox_pipe_menu>"
echo "  <item label=\"$( echo ${FILE} | sed s/_/__/g )\">"
echo "    <action name=\"Execute\">"
echo "      <command>viewnior '${FILE}'</command>"
echo "    </action>"
echo "  </item>"
echo "</openbox_pipe_menu>"

echo '1' >> ~/.stats/$( basename ${0} )
