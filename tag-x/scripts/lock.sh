#! /bin/sh

# ONE TIME TRY TO START mate-screensaver IF IT DOES NOT RUN
if pgrep -q mate-screensaver 1> /dev/null 2> /dev/null
then
  mate-screensaver 1> /dev/null 2> /dev/null &
fi

# FALLBACK TO xlockmore WHEN mate-screensaver DOES NOT RUN
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
