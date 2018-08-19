# This is my setup script for NixOS and Nix's home-manager.
# (Jonathan Reeve, jon.reeve@gmail.com)
# It first puts my NixOS configs in the right place using symlinks,
# then sets up fish shell functions and aliases that only need to be
# set up once.

set dots "$PWD/../dotfiles"
mkdir -p "$HOME/.config/nixpkgs"
ln -s "$dots/home.nix" "$HOME/.config/nixpkgs/home.nix"
sudo ln -s "$dots/configuration.nix" "$dots/hardware-configuration.nix" "/etc/nixos"

set -U EDITOR "emacsclient -c"
set -U BROWSER qutebrowser
set -U LANG en_US.UTF-8
set -U LC_ALL en_US.UTF-8
#set -U LOCALE_ARCHIVE=$HOME/.nix-profile/lib/locale/locale-archive
#set -U LOCALE_ARCHIVE="(readlink ~/.nix-profile/lib/locale)/locale-archive"

# Git abbreviations
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

abbr -a em emacsclient -c

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
