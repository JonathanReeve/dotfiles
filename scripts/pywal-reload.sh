#!/run/current-system/sw/bin/bash

# Source the colors.
. "${HOME}/.cache/wal/colors.sh"

# Set up qutebrowser.

echo "
c.colors.tabs.even.bg = \"$color1\"
c.colors.tabs.odd.bg = \"$color1\"
c.colors.tabs.selected.even.bg = \"$color2\"
c.colors.tabs.selected.odd.bg = \"$color2\"
" > /tmp/config.py

# Waybar
cat ~/.cache/wal/colors-waybar.css ~/.config/waybar/style.css > /tmp/waybar.css
pkill waybar
exec waybar -s /tmp/waybar.css

running=`ps cax | grep qutebrowser | wc -l`
if [ $running -gt 0 ]; then
    qutebrowser ":config-source /tmp/config.py"
fi

# Trigger homepage reload
# ./homepage/homepage.sh

# Trigger emacs reload
emacsclient --eval "(load-theme 'ewal-doom-one)"
