#!/usr/bin/env nu

def main [] {
    let info = (hyprctl getoption general:gaps_in -j | from json)
    
    # info is a record. Check 'int' or 'custom'.
    let gaps_val = if ("int" in $info) {
        $info.int
    } else if ("custom" in $info) {
        ($info.custom | split row " " | first | into int)
    } else {
        0
    }

    if $gaps_val == 0 {
        # Restore gaps
        hyprctl keyword general:gaps_in 5
        hyprctl keyword general:gaps_out 10
        hyprctl keyword decoration:rounding 11
    } else {
        # Remove gaps
        hyprctl keyword general:gaps_in 0
        hyprctl keyword general:gaps_out 0
        hyprctl keyword decoration:rounding 0
    }
}
