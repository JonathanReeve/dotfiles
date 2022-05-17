def em [f] {emacsclient -c $f &; disown}
def wal-fav [] {open ~/.cache/wal/colors.json | get wallpaper | each { echo $it (char newline)} | str collect | save -a ~/.cache/wal/favs}
def wal-fav-set [] {wal -i (open ~/.cache/wal/favs | lines | shuffle | keep 1)}
def wal-recent [] {wal -i (ls /run/media/jon/systemrestore/.systemrestore/Bildoj | where type == 'File' | sort-by modified -r | keep 50 | shuffle | keep 1 | get name)}
def wal-backup [] {sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore}
let scripts = ~/Dotfiles/scripts
def proj [project] {
open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { qutebrowser $it.websites };
open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
}
zoxide init nushell --hook prompt | save ~/.zoxide.nu
source ~/.zoxide.nu
echo (starship prompt)
