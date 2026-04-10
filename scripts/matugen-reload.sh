#!/run/current-system/sw/bin/bash
# Matugen Post-Generation Reload Script
LOGFILE="$HOME/.cache/matugen-reload.log"
echo "--- $(date) --- Theme Change Detected ---" >> "$LOGFILE"

# Export Wayland environment variables if they are missing
if [ -z "$WAYLAND_DISPLAY" ]; then
    export WAYLAND_DISPLAY=wayland-1
fi

# 1. Reload Emacs
if /run/current-system/sw/bin/emacsclient --eval "(load-theme 'base16-dms t)" >> "$LOGFILE" 2>&1; then
    echo "Emacs theme reloaded." >> "$LOGFILE"
else
    echo "Emacs reload failed." >> "$LOGFILE"
fi

# 2. Reload qutebrowser
if pgrep qutebrowser > /dev/null; then
    # We use qutebrowser's IPC to reload config
    /etc/profiles/per-user/jon/bin/qutebrowser ':config-source ~/.cache/dms-qute-config.py' >> "$LOGFILE" 2>&1
    echo "qutebrowser reloaded." >> "$LOGFILE"
else
    echo "qutebrowser not running." >> "$LOGFILE"
fi
