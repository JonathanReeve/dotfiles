#!/usr/bin/env bash

# Change focus
dconf write '/org/gnome/shell/extensions/pop-shell/focus-down' "['<Alt>n']"
dconf write '/org/gnome/shell/extensions/pop-shell/focus-up' "['<Alt>e']"
dconf write '/org/gnome/shell/extensions/pop-shell/focus-left' "['<Alt>h']"
dconf write '/org/gnome/shell/extensions/pop-shell/focus-right' "['<Alt>i']"

# Ender edit mode
dconf write '/org/gnome/shell/extensions/pop-shell/tile-enter' "['<Super>x']"

# Edit mode required for these next ones
dconf write '/org/gnome/shell/extensions/pop-shell/tile-move-right' "['i']"
dconf write '/org/gnome/shell/extensions/pop-shell/tile-move-left' "['h']"
dconf write '/org/gnome/shell/extensions/pop-shell/tile-move-up' "['e']"
dconf write '/org/gnome/shell/extensions/pop-shell/tile-move-down' "['n']"
