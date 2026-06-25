#!/usr/bin/env nu

def main [--emacsclient: string, --qutebrowser: string] {
    # 1. Reload Emacs theme if running
    reload-emacs $emacsclient

    # 2. Reload Qutebrowser theme if running
    reload-qutebrowser $qutebrowser

    # 3. Reload active terminal colors (like foot) using ESC sequences
    reload-terminals
}

def reload-emacs [custom_bin: string] {
    let bin = if ($custom_bin != null and ($custom_bin | path exists)) {
        $custom_bin
    } else {
        let path = (which emacsclient | get 0?.path)
        if ($path != null and ($path | is-not-empty)) {
            $path
        } else {
            let fallback = "/etc/profiles/per-user/jon/bin/emacsclient"
            if ($fallback | path exists) {
                $fallback
            } else {
                ""
            }
        }
    }

    if ($bin | is-empty) {
        print "Emacs: emacsclient not found, skipping."
        return
    }

    # Check if responsive
    let res = (do { ^$bin "--eval" "t" } | complete)
    if $res.exit_code == 0 {
        try {
            ^$bin --eval "(load-theme (quote ewal-doom-one) t)"
            print "Emacs: theme reloaded successfully."
        } catch {
            print "Emacs: failed to reload theme."
        }
    } else {
        print "Emacs: server not running, skipping."
    }
}

def reload-qutebrowser [custom_bin: string] {
    let bin = if ($custom_bin != null and ($custom_bin | path exists)) {
        $custom_bin
    } else {
        let path = (which qutebrowser | get 0?.path)
        if ($path != null and ($path | is-not-empty)) {
            $path
        } else {
            let fallback = "/etc/profiles/per-user/jon/bin/qutebrowser"
            if ($fallback | path exists) {
                $fallback
            } else {
                "qutebrowser"
            }
        }
    }

    # Check if running robustly
    let pgrep_res = (run-external "pgrep" "-fa" "qutebrowser" | complete)
    let running = if $pgrep_res.exit_code == 0 {
        $pgrep_res.stdout 
        | lines 
        | where {|line| (not ($line | str contains "theme-reload")) and (not ($line | str contains "emacs")) and (not ($line | str contains "nvim")) and (not ($line | str contains "vim")) } 
        | is-not-empty
    } else {
        false
    }

    if $running {
        try {
            ^$bin ':config-source ~/.cache/dms-qute-config.py'
            print "Qutebrowser: config reloaded successfully."
        } catch {
            print "Qutebrowser: failed to reload config."
        }
    } else {
        print "Qutebrowser: not running, skipping."
    }
}

def reload-terminals [] {
    let colors_file = "~/.cache/wal/colors.json" | path expand
    if not ($colors_file | path exists) {
        print "Terminals: colors.json not found, skipping."
        return
    }

    let data = try {
        open $colors_file
    } catch {
        print "Terminals: failed to parse colors.json, skipping."
        return
    }

    let esc = (char -i 27)
    let bel = (char -i 7)

    # OSC 11: background, OSC 10: foreground, OSC 12: cursor
    let seq_bg = $"($esc)]11;($data.special.background)($bel)"
    let seq_fg = $"($esc)]10;($data.special.foreground)($bel)"
    let seq_cursor = $"($esc)]12;($data.special.cursor)($bel)"

    # OSC 4: palette colors 0..15
    let seq_palette = (0..15 | each {|i|
        let val = ($data.colors | get $"color($i)")
        $"($esc)]4;($i);($val)($bel)"
    } | str join)

    let sequences = $"($seq_bg)($seq_fg)($seq_cursor)($seq_palette)"

    # Send sequences to all active /dev/pts/* files
    let pts_files = (ls /dev/pts | where name =~ '/[0-9]+$' | get name)
    
    if ($pts_files | is-empty) {
        print "Terminals: no active terminal pts devices found."
        return
    }

    mut count = 0
    for pts in $pts_files {
        try {
            $sequences | save -r -f $pts
            $count = $count + 1
        }
    }
    print $"Terminals: reloaded colors for ($count) active terminal sessions."
}
