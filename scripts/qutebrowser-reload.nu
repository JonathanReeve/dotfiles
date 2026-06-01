#!/usr/bin/env nu

def main [] {
    # Check if qutebrowser is running
    let qb_running = (run-external "pgrep" "-f" "qutebrowser" | complete | get stdout | is-not-empty)
    
    if $qb_running {
        try {
            # Send the config-source command to the running instance
            ^qutebrowser ':config-source ~/.cache/dms-qute-config.py'
        } catch {
            print "Failed to reload qutebrowser configuration."
        }
    }
}
