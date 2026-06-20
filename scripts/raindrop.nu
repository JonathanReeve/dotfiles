#!/usr/bin/env nu

# A script to fetch Raindrop bookmarks
# Documentation: https://developer.raindrop.io/
def main [
    --token: string  # Raindrop API token
    --collection: int = 0 # Collection ID (0 for all)
] {
    let api_token = if ($token | is-empty) {
        # Try to get from pass if not provided
        (run-external "pass" "show" "raindrop.io/token" | complete | get stdout | str trim)
    } else {
        $token
    }

    if ($api_token | is-empty) {
        print "Error: Raindrop API token not found. Provide via --token or 'pass raindrop.io/token'."
        return
    }

    print $"Fetching Raindrop bookmarks from collection ($collection)..."
    
    # Fetch recent bookmarks
    let response = (http get 
        --headers [Authorization $"Bearer ($api_token)"]
        $"https://api.raindrop.io/rest/v1/raindrops/($collection)"
    )

    if ($response.result) {
        let items = ($response.items | select title link excerpt type tags created)
        return $items
    } else {
        print "Failed to fetch bookmarks."
        return $response
    }
}
