# Handy aliases
def em [f] {emacsclient -c $f &; disown}

# Wallpaper management
def wal-fav [] {open ~/.cache/wal/colors.json | get wallpaper | each { |it| echo $it (char newline)} | str collect | save --append ~/.cache/wal/favs}
def wal-fav-set [] {wal -i (open ~/.cache/wal/favs | lines | shuffle | first )}
def wal-recent [] {wal -i (ls /run/media/jon/systemrestore/.systemrestore/Bildoj | where type == 'File' | sort-by modified -r | first 50 | shuffle | first | get name)}
def wal-backup [] {sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore}

# Project managemnt
def proj [project] {
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { |it| qutebrowser $it.websites };
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
}

# Jump around with z
zoxide init nushell --hook prompt | save ~/.zoxide.nu
source ~/.zoxide.nu

# Starship config from https://www.nushell.sh/book/3rdpartyprompts.html#starship
let-env STARSHIP_SHELL = "nu"

def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Use nushell functions to define your right and left prompt
let-env PROMPT_COMMAND = { create_left_prompt }
let-env PROMPT_COMMAND_RIGHT = ""

# The prompt indicators are environmental variables that represent
# the state of the prompt
let-env PROMPT_INDICATOR = ""
let-env PROMPT_INDICATOR_VI_INSERT = ": "
let-env PROMPT_INDICATOR_VI_NORMAL = "〉"
let-env PROMPT_MULTILINE_INDICATOR = "::: "
