#!/usr/bin/env nu

def main [] {
    let emacsclient = (which emacsclient | get 0?.path)
    
    if ($emacsclient | is-empty) {
        print "Error: emacsclient not found in PATH"
        return
    }

    # Check if Emacs server is responsive
    let server_res = (do { ^$emacsclient "--eval" "t" } | complete)
    
    if $server_res.exit_code == 0 {
        try {
            # Just reload the theme
            ^$emacsclient --eval "(load-theme (quote ewal-doom-one) t)"
        } catch {
            print "Failed to reload Emacs theme."
        }
    }
}
