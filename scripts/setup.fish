# This is my setup script for NixOS and Nix's home-manager.
# (Jonathan Reeve, jon.reeve@gmail.com)
# It first puts my NixOS configs in the right place using symlinks,
# then sets up fish shell functions and aliases that only need to be
# set up once.

set dots "/home/jon/Dotfiles/dotfiles"
mkdir -p "$HOME/.config/nixpkgs"
ln -s "$dots/home.nix" "$HOME/.config/nixpkgs/home.nix"
sudo ln -s "$dots/configuration.nix" "$dots/hardware-configuration.nix" "/etc/nixos"



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
