set -U EDITOR vim
set -U BROWSER firefox

abbr -a ga git add
abbr -a gc git commit
abbr -a gcam git commit -am 
abbr -a gcm git commit -m
abbr -a gco git checkout
abbr -a gcob git checkout -b
abbr -a gcom git checkout master
abbr -a gd git diff
abbr -a gp git push
abbr -a gc git commit
abbr -a gcam git commit -am 
abbr -a gcm git commit -m
abbr -a gd git diff
abbr -a gdc git diff --cached
abbr -a glg git lg
abbr -a gcod git checkout develop
abbr -a gst git status

abbr -a install sudo pacman -S
abbr -a search pacman -Ss
abbr -a update yaourt -Syu --aur --noconfirm

#opens password file
abbr -a pw vim ~/Dropbox/Personal/.p10.txt

set -U vaultmount ~/Documents/Settings/.private-mount
set -U vaultloc ~/Dropbox/Personal/.Vault_encfs 

alias vault="encfs $vaultloc $vaultmount"
alias unvault="fusermount -u $vaultmount"
funcsave vault
funcsave unvault

function jnl
	vault
	and emacsclient -c $vaultmount/Journal/jnl.org
	and unvault
end
funcsave jnl
