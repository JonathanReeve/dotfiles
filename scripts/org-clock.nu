#!/usr/bin/env nu

def main [] {
    let emacsclient = "/etc/profiles/per-user/jon/bin/emacsclient"
    
    try {
        let result = (^$emacsclient --eval "(if (org-clock-is-active) (format \"%s (%d:%02d)\" (or org-clock-heading \"Task\") (/ (org-clock-get-clocked-time) 60) (% (org-clock-get-clocked-time) 60)) \"-1\")" | str trim)
        
        # Strip surrounding quotes from elisp string output
        let clock_str = ($result | str replace -r '^"' '' | str replace -r '"$' '')
        
        let output = if $clock_str == "-1" or $clock_str == "" {
            "Protocolu!"
        } else {
            $clock_str
        }

        print $output

        # Update Busybar via Nix-shebang busybar.py script
        let busybar_script = "/home/jon/Agordoj/scripts/busybar.py"
        if ($busybar_script | path exists) {
            try {
                ^$busybar_script update $output | ignore
            } catch {}
        }
    } catch {
        print "Ensalutu!"
    }
}
