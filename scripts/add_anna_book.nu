#!/usr/bin/env nu

# add_anna_book.nu
# Fetches metadata from Anna's Archive and triggers Emacs note creation.
# Renames and moves book files to storage using a generated citekey.

# --- Configuration ---
const ROAM_DIR = "/home/jon/Dokumentoj/Org/Roam"
const PAPERS_DIR = "/home/jon/Dokumentoj/Papers"
const SCAN_DIRS = [ "~/Elŝutoj/" "/tmp/" ]

const ANNA_API_BASE = "https://annas-archive.gl/db/aarecord_elasticsearch/md5:"
const ANNA_WEB_BASE = "https://annas-archive.gl/md5/"

const PAYLOAD_FILE = "/tmp/anna_payload.json"
const HELPER_EL = "/home/jon/Agordoj/scripts/anna-vulpea-helper.el"
# ---------------------

def generate_citekey [author: string, year: string, title: string] {
    # Extract first author's last name
    let author_clean = (
        if ($author | is-empty) { 
            "unknown" 
        } else {
            let first_author = ($author | split row ";" | get 0)
            if ($first_author =~ ",") {
                $first_author | split row "," | get 0 | str trim
            } else {
                # Handle cases like "Sarah Messer"
                let parts = ($first_author | str trim | split row " ")
                if ($parts | length) > 1 {
                    $parts | last
                } else {
                    $parts | get 0
                }
            }
        } | str lowercase | str replace --all --regex `[^a-z0-9]` ""
    )

    let year_clean = ($year | str replace --all --regex `[^0-9]` "" | str substring 0..4)
    
    # Extract first significant word of title
    let title_clean = (
        if ($title | is-empty) {
            "book"
        } else {
            $title 
            | str lowercase 
            | split row --regex `[\s\-_/]+` 
            | where ($it != "the" and $it != "a" and $it != "an" and $it != "of" and $it != "and") 
            | get 0? 
            | default "book"
            | str replace --all --regex `[^a-z0-9]` ""
        }
    )

    $"($author_clean)($year_clean)($title_clean)"
}

def main [
    ...targets: string # Specific books to process (paths, filenames, or MD5s)
    --all (-a)  # Process all matching books found (default: only the most recent)
    --dry-run (-d) # Show what would be done without making changes
] {
    let meta_dir = ([$ROAM_DIR "Metadata"] | path join)
    let cookie_db = ([ $env.HOME ".local" "share" "qutebrowser" "webengine" "Cookies" ] | path join)
    
    if not ($meta_dir | path exists) { mkdir $meta_dir }
    if not ($PAPERS_DIR | path exists) { mkdir $PAPERS_DIR }

    if not ($cookie_db | path exists) {
        error make {msg: $"Cookie database not found at ($cookie_db)"}
    }

    let host = ($ANNA_API_BASE | url parse | get host)
    let cookies_list = (sqlite3 $cookie_db $"SELECT name, value FROM cookies WHERE host_key LIKE '%($host)%'" 
        | lines 
        | parse "{name}|{value}")

    if not ($cookies_list | any { |c| $c.name == "aa_account_id2" }) {
        error make {msg: $"No aa_account_id2 cookie found for ($host) in qutebrowser. Please log in to Anna's Archive in qutebrowser."}
    }

    let cookie_header = ($cookies_list | each { |row| $"($row.name)=($row.value)" } | str join "; ")

    # Search for files with 32-char hex MD5 in name
    mut books = []
    for d in $SCAN_DIRS {
        let expanded = ($d | path expand)
        if ($expanded | path exists) {
            let found = (ls $expanded | where name =~ `\.(pdf|epub)$`)
            for f in $found {
                let md5_list = ($f.name | parse --regex "([a-fA-F0-9]{32})")
                if ($md5_list | length) > 0 {
                    $books = ($books | append { 
                        path: $f.name, 
                        filename: ($f.name | path basename),
                        extension: ($f.name | path parse | get extension),
                        md5: ($md5_list | get 0.capture0),
                        modified: $f.modified
                    })
                }
            }
        }
    }
    
    $books = ($books | sort-by modified)

    mut to_process = []
    if ($targets | is-empty) {
        if ($books | is-empty) {
            print "No books with MD5 in filename found."
            return
        }
        $to_process = if $all { $books } else { [($books | last)] }
    } else {
        for target in $targets {
            let expanded = ($target | path expand)
            if ($expanded | path exists) {
                if (ls $expanded | get 0.type) == "file" {
                    let md5_list = ($expanded | parse --regex "([a-fA-F0-9]{32})")
                    if ($md5_list | length) > 0 {
                        $to_process = ($to_process | append {
                            path: $expanded,
                            filename: ($expanded | path basename),
                            extension: ($expanded | path parse | get extension),
                            md5: ($md5_list | get 0.capture0),
                            modified: (ls $expanded | get 0.modified)
                        })
                    } else {
                        print $"Warning: File '($target)' does not contain a 32-character hex MD5 in its name."
                    }
                } else {
                    print $"Warning: '($target)' is not a file."
                }
            } else {
                # Try to match in the scanned books
                let matched = ($books | where filename =~ $target or path =~ $target or md5 =~ $target)
                if ($matched | is-empty) {
                    print $"Warning: Could not find any book matching '($target)'."
                } else {
                    $to_process = ($to_process | append $matched)
                }
            }
        }
        $to_process = ($to_process | uniq)
    }

    if ($to_process | is-empty) {
        print "No books to process."
        return
    }

    for book in $to_process {
        if ($book | is-empty) { continue }
        let md5 = $book.md5
        let filename = $book.filename
        print $"Processing ($filename)..."

        # Check if already exists in Roam by searching content for MD5
        let existing = (grep -l $":MD5: ($md5)" ...(glob $"($ROAM_DIR)/*.org") | complete)
        if ($existing.exit_code == 0) {
            let existing_path = ($existing.stdout | lines | first)
            print $"  Skipping Note: Already exists at ($existing_path)"
            continue
        }

        # Fetch metadata (prefer local cache if exists)
        let raw_meta_path = ([$meta_dir $"($md5).json"] | path join)
        let metadata = (
            if ($raw_meta_path | path exists) {
                print "  Using cached metadata."
                open $raw_meta_path
            } else {
                let url = $"($ANNA_API_BASE)($md5).json"
                let headers = {Cookie: $cookie_header}
                try {
                    let res = (http get --headers $headers $url)
                    $res | to json | save --force $raw_meta_path
                    print $"  Fetched and saved metadata to ($raw_meta_path)"
                    sleep 3sec
                    $res
                } catch { |err|
                    let is_404 = (try { ($err | to text) =~ "404" } catch { false })
                    if $is_404 {
                        print $"  Not found on Anna's Archive (404)."
                    } else {
                        print "  Error fetching metadata:"
                        print $err
                    }
                    sleep 3sec
                    continue
                }
            }
        )

        let data = ($metadata | get file_unified_data?)
        if ($data == null) {
            print "  No unified data found in metadata."
            continue
        }

        # Extract identifiers and descriptive fields
        let idents = ($data | get identifiers_unified)
        let title = ($data | get title_best | default "Untitled" | str replace --all "/" "-")
        let author = ($data | get author_best | default "Unknown Author")
        let year = ($data | get year_best | default "")
        let publisher = ($data | get publisher_best | default "")
        let language = ($data | get language_codes | get 0? | default "en")
        let description = ($data | get stripped_description_best? | default "")
        
        # Generate Citekey
        let cite_key = (generate_citekey $author $year $title)
        print $"  Generated Citekey: ($cite_key)"

        # Collect external identifiers
        let isbn13 = ($idents | get isbn13? | get 0? | default "")
        let goodreads_id = ($idents | get goodreads? | get 0? | default "")
        let gbooks_id = ($idents | get gbooks? | get 0? | default "")
        let ol_id = ($idents | get ol? | get 0? | default "")
        let oclc_id = ($idents | get oclc? | get 0? | default "")
        let asin_id = ($idents | get asin? | get 0? | default "")
        
        # Search raw text for Wikidata QID / Gutenberg
        let raw_text = ($metadata | to json)
        let wikidata_id = (if ($raw_text =~ `\"Q[0-9]+\"`) { ($raw_text | parse --regex `\"(Q[0-9]+)\"` | get 0.capture0) } else { "" })
        let gutenberg_id = (if ($raw_text =~ `\"gutenberg:[0-9]+\"`) { ($raw_text | parse --regex `\"gutenberg:([0-9]+)\"` | get 0.capture0) } else { "" })

        # URLs
        mut extra_urls = []
        if ($goodreads_id != "") { $extra_urls = ($extra_urls | append $"https://www.goodreads.com/book/show/($goodreads_id)") }
        if ($gbooks_id != "") { $extra_urls = ($extra_urls | append $"https://books.google.com/books?id=($gbooks_id)") }
        if ($ol_id != "") { $extra_urls = ($extra_urls | append $"https://openlibrary.org/works/($ol_id)") }
        if ($oclc_id != "") { $extra_urls = ($extra_urls | append $"https://www.worldcat.org/oclc/($oclc_id)") }
        if ($asin_id != "") { $extra_urls = ($extra_urls | append $"https://www.amazon.com/dp/($asin_id)") }
        if ($wikidata_id != "") { $extra_urls = ($extra_urls | append $"https://www.wikidata.org/wiki/($wikidata_id)") }
        if ($gutenberg_id != "") { $extra_urls = ($extra_urls | append $"https://www.gutenberg.org/ebooks/($gutenberg_id)") }
        $extra_urls = ($extra_urls | append $"($ANNA_WEB_BASE)($md5)")

        # File management: rename and move
        let target_filename = $"($cite_key).($book.extension)"
        let target_path = ([$PAPERS_DIR $target_filename] | path join)
        
        if $dry_run {
            print $"--- DRY RUN ---"
            print $"  Would move: ($book.path) -> ($target_path)"
            let emacs_payload = {
                citekey: $cite_key
                title: $title
                author: $author
                url: ($extra_urls | str join " ")
                noter_doc: $target_path
                md5: $md5
                isbn: $isbn13
                lang: $language
                publisher: $publisher
                year: $year
                description: $description
            }
            print "  Emacs Payload:"
            print ($emacs_payload | table -e)
        } else {
            # Only move if it hasn't been moved yet
            if not ($target_path | path exists) {
                mv $book.path $target_path
                print $"  Moved book to ($target_path)"
            } else {
                print $"  Book already exists at ($target_path)"
            }
            
            let emacs_payload = {
                citekey: $cite_key
                title: $title
                author: $author
                url: ($extra_urls | str join " ")
                noter_doc: $target_path
                md5: $md5
                isbn: $isbn13
                lang: $language
                publisher: $publisher
                year: $year
                description: $description
            }
            $emacs_payload | to json | save --force $PAYLOAD_FILE
            
            # Use emacsclient to trigger note creation via helper
            let elisp = (["(progn (load \"" $HELPER_EL "\") (my/vulpea-create-anna-note \"" $PAYLOAD_FILE "\"))"] | str join)
            print "  Triggering Emacs note creation..."
            let res = (emacsclient --eval $elisp | complete)
            if $res.exit_code != 0 {
                print $"  Error triggering Emacs: ($res.stderr)"
            } else {
                print "  Note created and opened in Emacs."
            }
        }
    }
}
