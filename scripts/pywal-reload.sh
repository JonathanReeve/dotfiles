#!/usr/bin/bash

# Source the colors.
. "${HOME}/.cache/wal/colors.sh"

# Set up qutebrowser.

echo "
c.colors.tabs.even.bg = \"$color1\"
c.colors.tabs.odd.bg = \"$color1\"
c.colors.tabs.selected.even.bg = \"$color2\"
c.colors.tabs.selected.odd.bg = \"$color2\"
" > config.py

running=`ps cax | grep qutebrowser | wc -l`
if [ $running -gt 0 ]; then
    qutebrowser ":config-source $PWD/config.py"
fi

# Trigger homepage reload
./homepage/homepage.sh
