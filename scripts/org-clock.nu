#!/usr/bin/env nu

def main [] {
    let emacsclient = "/etc/profiles/per-user/jon/bin/emacsclient"
    
    try {
        let result = (^$emacsclient --eval "(if (org-clocking-p) (org-clock-get-clock-string) -1)" | str trim)
        
        if $result == "-1" {
            print "Protocolu!"
        } else {
            # Extract content between double quotes
            let clock_str = ($result | split row '"' | get 1)
            print $clock_str
        }
    } catch {
        print "Ensalutu!"
    }
}
