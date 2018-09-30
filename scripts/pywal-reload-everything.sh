#!/run/current-system/sw/bin/bash

# Source the colors.
. "${HOME}/.cache/wal/colors.sh"

# Set up BSPWM

# Set the border colors.
bspc config normal_border_color "$color1"
bspc config active_border_color "$color3"
bspc config focused_border_color "$color2"
bspc config presel_feedback_color "$color3"

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
