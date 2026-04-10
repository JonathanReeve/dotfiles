#!/run/current-system/sw/bin/bash
scripts="/home/jon/Agordoj/scripts"
wallpaper=$(find /home/jon/Bildoj/Ekranfonoj -type f | shuf -n 1)

echo "Setting wallpaper: $wallpaper"

# 1. Manually run matugen with your custom config.
# This ensures templates for Emacs, Alacritty, and Qutebrowser are generated.
matugen image "$wallpaper" --config /home/jon/.config/matugen/config.toml

# 2. Tell DMS to change the wallpaper (for the bar/notifications)
# We don't use 'queue' here to avoid triggering the crashing worker
dms ipc call wallpaper set "$wallpaper"

# 3. Explicitly trigger reloads just in case matugen hooks fail
emacsclient --eval "(load-theme 'base16-dms t)"
if pgrep qutebrowser > /dev/null; then
    qutebrowser ":config-source /tmp/dms-qute-config.py"
fi
