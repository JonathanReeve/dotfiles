#!/usr/bin/env nu

def main [] {
    let window = (hyprctl activewindow -j | from json)
    
    if ($window | is-empty) { return }

    if $window.floating {
        # Currently floating, just toggle back to tiling
        hyprctl dispatch togglefloating
    } else {
        # Currently tiling, float it and set size/position
        hyprctl dispatch togglefloating
        hyprctl dispatch resizeactive exact 70% 70%
        hyprctl dispatch centerwindow
    }
}
