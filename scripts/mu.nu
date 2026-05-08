#!/usr/bin/env nu

# A helper script for mu email operations
def main [] {
    help mu.nu
}

# Search for emails using mu
export def "main search" [
    query: string,      # Mu query string
    --maxnum (-n): int = 10  # Maximum number of results
] {
    let raw = (mu find --maxnum $maxnum --format json $query)
    if ($raw | is-empty) {
        print "No messages found."
        return
    }

    let messages = ($raw | from json)
    
    $messages | select ":date-unix" ":from" ":subject" ":maildir" ":path" 
        | rename date from subject maildir path
        | upsert date { |row| $row.date | into datetime }
        | upsert from { |row| $row.from | each { |f| $"($f.':name') <($f.':email')>" } | str join ", " }
}

# View an email using mu
export def "main view" [
    path: string        # Path to the email file
    --lines (-l): int = 100 # Maximum number of lines to display
] {
    let content = (mu view $path)
    let lines_list = ($content | lines)
    if ($lines_list | length) > $lines {
        $lines_list | take $lines | str join "\n"
        print $"\n... (truncated (($lines_list | length) - $lines)) lines) ..."
    } else {
        $content
    }
}

# Move a message to a destination maildir
export def "main move" [
    path: string,       # Path to the email file
    destination: string # Destination maildir (e.g., /Archive)
] {
    mu move $path $destination
    print $"Moved ($path) to ($destination)"
}

# Archive an email (move to /Archive)
export def "main archive" [
    path: string        # Path to the email file
] {
    main move $path "/Archive"
}

# Delete an email (move to /Trash)
export def "main delete" [
    path: string        # Path to the email file
] {
    main move $path "/Trash"
}

# Create a TODO item in Org-mode linked to the email
export def "main todo" [
    path: string        # Path to the email file
] {
    let raw = (mu view --format sexp $path)
    # Basic parsing for S-expression key-value pairs
    let subject = ($raw | str replace -r '.*:subject\s+"([^"]+)".*' '$1')
    let msgid = ($raw | str replace -r '.*:message-id\s+"([^"]+)".*' '$1')
    
    # Extract sender name or email
    let from_match = ($raw | parse -r ':from \(\((?P<fields>[^)]+)\)\)')
    let from = if ($from_match | is-empty) {
        "Unknown"
    } else {
        let fields = ($from_match.fields.0)
        if ($fields | str contains ":name") {
            $fields | str replace -r '.*:name\s+"([^"]+)".*' '$1'
        } else {
            $fields | str replace -r '.*:email\s+"([^"]+)".*' '$1'
        }
    }

    let link = $"mu4e:msgid:($msgid)"
    let title = $"Email from ($from): ($subject)"
    
    # Use org-protocol to capture
    let url = $"org-protocol://capture?template=t&url=($link | url encode)&title=($title | url encode)"
    # We use emacsclient to trigger the capture
    run-external "emacsclient" $url
    print $"Sent TODO capture request for: ($title)"
}

# Add a calendar entry and archive the email
export def "main calendar" [
    path: string        # Path to the email file
] {
    let raw = (mu view --format sexp $path)
    let subject = ($raw | str replace -r '.*:subject\s+"([^"]+)".*' '$1')
    let msgid = ($raw | str replace -r '.*:message-id\s+"([^"]+)".*' '$1')
    let link = $"mu4e:msgid:($msgid)"
    
    # Use org-protocol schedule template
    let url = $"org-protocol://capture?template=s&url=($link | url encode)&title=($subject | url encode)"
    run-external "emacsclient" $url
    
    # Archive after capturing
    main archive $path
}
