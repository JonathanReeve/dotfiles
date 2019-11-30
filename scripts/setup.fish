# This is my setup script for NixOS and Nix's home-manager.
# (Jonathan Reeve, jon.reeve@gmail.com)
# It first puts my NixOS configs in the right place using symlinks,
# then sets up fish shell functions and aliases that only need to be
# set up once.

set dots "/home/jon/Dotfiles/dotfiles"
mkdir -p "$HOME/.config/nixpkgs"
ln -sf "$dots/home.nix" "$XDG_CONFIG_HOME/nixpkgs/home.nix"
sudo ln -sf "$dots/configuration.nix" "/etc/nixos"
sudo ln -sf "$dots/hardware-configuration.nix" "/etc/nixos"
ln -sf "/home/jon/Dotfiles/scripts/org-clock.sh" "$XDG_CONFIG_HOME/argos/clock.30s.sh"
