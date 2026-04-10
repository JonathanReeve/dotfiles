#!/run/current-system/sw/bin/bash
scripts="/home/jon/Agordoj/scripts"
# Get the dominant hex color from pywal cache
primary_color=$(head -n 1 ~/.cache/wal/colors)
python3 "$scripts/dms-reload.py" "$primary_color"
