# Handy aliases
def em [f] {emacsclient -c $f &; disown}

# Wallpaper management
def wal-fav [] {
  open ~/.cache/wal/colors.json | get wallpaper |
  each { |it| echo $it (char newline)} |
  str collect | save --append ~/.cache/wal/favs
}

def wal-fav-set [] {
  let w = (open ~/.cache/wal/favs | lines | uniq | shuffle | first)
  echo $"Using ($w)"
  pkill swaybg
  swaybg -o 'DP-6' -i $w -m fill
}

def wal-recent [] {wal -i (ls /run/media/jon/systemrestore/.systemrestore/Bildoj | sort-by modified -r | first 50 | shuffle | first | get name)}
def wal-backup [] {sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore}

# Project managemnt
def proj [project] {
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { |it| qutebrowser $it.websites };
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
}

# Jump around with z
# zoxide init nushell --hook prompt | save ~/.zoxide.nu
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

# https://raw.githubusercontent.com/nushell/nu_scripts/main/background_task/job.nu
# spawn task to run in the background
#
# please note that the it spawned a fresh nushell to execute the given command
# So it doesn't inherit current scope's variables, custom commands, alias definition, except env variables which value can convert to string.
#
# e.g:
# spawn { echo 3 }
export def spawn [
    command: block   # the command to spawn
] {
    let config_path = $nu.config-path
    let env_path = $nu.env-path
    let source_code = (view-source $command | str trim -l -c '{' | str trim -r -c '}')
    let job_id = (pueue add -p $"nu --config \"($config_path)\" --env-config \"($env_path)\" -c '($source_code)'")
    {"job_id": $job_id}
}

export def log [
    id: int   # id to fetch log
] {
    pueue log $id -f --json
    | from json
    | transpose -i info
    | flatten --all
    | flatten --all
    | flatten status
}

# get job running status
export def status () {
    pueue status --json
    | from json
    | get tasks
    | transpose -i status
    | flatten
    | flatten status
}

# kill specific job
export def kill (id: int) {
    pueue kill $id
}

# clean job log
export def clean () {
    pueue clean
}
