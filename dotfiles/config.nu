
# spawn task to run in the background
use ~/Agordoj/scripts/nu_scripts/modules/background_task/task.nu

# Wallpaper management
def wal-fav [] {
  open ~/.cache/wal/colors.json | get wallpaper |
  each { |it| echo $it (char newline)} |
  str join | save --append ~/.cache/wal/favs
}

def wal-fav-set [] {
  task spawn -i -l swaybg {
    ^pkill swaybg
    task kill
    let w = (open ~/.cache/wal/favs | lines | uniq | shuffle | first)
    echo $"Using ($w)"
    swaybg -i $w -m fill
  }
}

def wal-recent [] {
  wal -i (ls /run/media/jon/systemrestore/.systemrestore/Bildoj
         | sort-by modified -r
         | first 50
         | shuffle
         | first
         | get name)
}

def wal-backup [] {
  sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore
}

# Project managemnt
def proj [project] {
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { |it| qutebrowser $it.websites };
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
}

def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Handy aliases
def em [f] { spawn { emacsclient -c $f } }
