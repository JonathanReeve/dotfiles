#!/usr/bin/env nu

def main [] {
    let emacsclient = "/etc/profiles/per-user/jon/bin/emacsclient"
    
    # Check if Emacs server is responsive
    let server_res = (run-external $emacsclient "--eval" "t" | complete)
    
    if $server_res.exit_code == 0 {
        try {
            # Just reload the theme
            ^$emacsclient --eval "(load-theme (quote ewal-doom-one) t)"
        } catch {
            print "Failed to reload Emacs theme."
        }
    }
}
