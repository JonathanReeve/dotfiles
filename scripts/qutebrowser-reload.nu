#!/usr/bin/env nu

def main [] {
    # pgrep -f is more reliable for identifying qutebrowser
    let qb_running = (run-external "pgrep" "-f" "qutebrowser" | complete | get stdout | is-not-empty)
    
    if $qb_running {
        try {
            # Send the config-source command to the running instance
            # We use 'sh -c' to ensure the binary is found in the PATH if needed
            sh -c "qutebrowser ':config-source ~/.cache/dms-qute-config.py' || true"
        } catch {
            print "Failed to reload qutebrowser configuration."
        }
    }
}
