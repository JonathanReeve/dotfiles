#!/etc/profiles/per-user/jon/bin/nu

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Save Link
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🤖
# @raycast.packageName Org Protocol

# Documentation:
# @raycast.description Save a link in emacs using org-protocol
# @raycast.author Jonathan Reeve
# @raycast.authorURL http://jonreeve.com

# Get data from Safari and clipboard
let url = (osascript -e 'tell application "Safari" to get URL of current tab of front window' | str trim)
let title = (osascript -e 'tell application "Safari" to get name of current tab of front window' | str trim)
let selection = (pbpaste | str trim)

# URL encode using built-in Nushell command
let encoded_url = ($url | url encode)
let encoded_title = ($title | url encode)
let encoded_selection = ($selection | url encode)

# Construct org-protocol URL
# let org_url = $"org-protocol://store-link?url=($encoded_url)&title=($encoded_title)&selection=($encoded_selection)"
let template_key = "i"  # Replace with your org-capture template key
let org_url = $"org-protocol://capture?template=($template_key)&url=($encoded_url)&title=($encoded_title)&body=($encoded_selection)"

# Send to Emacs
/etc/profiles/per-user/jon/bin/emacsclient $org_url

# Confirm
echo $"Sent to Org: ($title)"
