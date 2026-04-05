#!/run/current-system/sw/bin/bash
scripts="/home/jon/Agordoj/scripts"
wallpaper=$(find /home/jon/Bildoj/Ekranfonoj -type f | shuf -n 1)

# Generate theme with matugen via DMS. 
# This will trigger our custom templates for Emacs, Alacritty, and Qutebrowser.
dms matugen queue --value "$wallpaper" --kind image

# Wait a brief moment for templates to write
sleep 1

# Reload Emacs theme
emacsclient --eval "(load-theme 'base16-dms t)"

# Reload qutebrowser config
if pgrep qutebrowser > /dev/null; then
    qutebrowser ":config-source /tmp/dms-qute-config.py"
fi
