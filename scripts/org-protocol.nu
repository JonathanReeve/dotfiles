#!/etc/profiles/per-user/jon/bin/nu

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Org-Protocol
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🤖
# @raycast.packageName Org Protocol

# Documentation:
# @raycast.description Open a link in emacs using org-protocol
# @raycast.author Jonathan Reeve
# @raycast.authorURL http://jonreeve.com

# Get URL and Title (example for Chrome using AppleScript via osascript)
# Note: You might need to grant permissions for Raycast/Terminal to control Chrome.
URL=$(osascript -e 'tell application "Safari" to get URL of current tab of front window')
TITLE=$(osascript -e 'tell application "Safari" to get name of current tab of front window')
SELECTION=$(pbpaste) # Or get selection via other means

# URL Encode (macOS has python, or you can use other tools)
ENCODED_URL=$(python -c "import urllib.parse; print(urllib.parse.quote(input()))" <<< "$URL")
ENCODED_TITLE=$(python -c "import urllib.parse; print(urllib.parse.quote(input()))" <<< "$TITLE")
ENCODED_SELECTION=$(python -c "import urllib.parse; print(urllib.parse.quote(input()))" <<< "$SELECTION")

# Construct the org-protocol URL
ORG_URL="org-protocol://store-link?url=${ENCODED_URL}&title=${ENCODED_TITLE}&selection=${ENCODED_SELECTION}"

# Open the URL
emacsclient "${ORG_URL}"

echo "Sent to Org: ${TITLE}"
