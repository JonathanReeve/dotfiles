#!/usr/bin/env nu

# A helper script for Hyprland operations
def main [] {
    help hypr.nu
}

# Toggle floating state and center window if floating
export def "main float-toggle" [] {
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

# Toggle gaps and rounding (clean vs focused look)
export def "main gaps-toggle" [] {
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
        # Restore gaps and rounded/goth look
        hyprctl keyword general:gaps_in 5
        hyprctl keyword general:gaps_out 10
        hyprctl keyword decoration:rounding 11
        
        # Restore DMS settings
        dms ipc call settings set cornerRadius 11
        dms ipc call settings set gothCornersEnabled true
    } else {
        # Remove gaps and make everything square
        hyprctl keyword general:gaps_in 0
        hyprctl keyword general:gaps_out 0
        hyprctl keyword decoration:rounding 0
        
        # DMS square look
        dms ipc call settings set cornerRadius 0
        dms ipc call settings set gothCornersEnabled false
    }

    # Force a global layout refresh by "switching" to the current workspace
    hyprctl dispatch workspace e+0
}

# Toggle between dwindle and scrolling layouts
export def "main layout-toggle" [] {
    let current_layout = (hyprctl getoption general:layout -j | from json | get str)
    
    if $current_layout == "dwindle" {
        hyprctl keyword general:layout scrolling
    } else {
        hyprctl keyword general:layout dwindle
    }
}
