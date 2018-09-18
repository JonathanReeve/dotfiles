#!/bin/sh

# Source the colors.
. "${HOME}/.cache/wal/colors.sh"

# Set the border colors.
bspc config normal_border_color "$color1"
bspc config active_border_color "$color2"
bspc config focused_border_color "$color15"
bspc config presel_feedback_color "$color1"
