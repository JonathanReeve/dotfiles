#!/usr/bin/env nu

def main [] {
    let current_layout = (hyprctl getoption general:layout -j | from json | get str)
    
    if $current_layout == "dwindle" {
        hyprctl keyword general:layout scrolling
    } else {
        hyprctl keyword general:layout dwindle
    }
}
