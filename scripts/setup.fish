# This is my setup script for NixOS and Nix's home-manager.
# It first puts my NixOS configs in the right place using symlinks,
# then sets up fish shell functions and aliases that only need to be
# set up once.

set dots "/home/jon/Dotfiles/dotfiles"
set XDG_CONFIG_HOME /home/jon/.config
mkdir -p "$HOME/.config/nixpkgs"
ln -sf "$dots/home.nix" "$XDG_CONFIG_HOME/nixpkgs/home.nix"
sudo ln -sf "$dots/configuration.nix" "/etc/nixos"
sudo ln -sf "$dots/hardware-configuration.nix" "/etc/nixos"
ln -sf "/home/jon/Dotfiles/scripts/org-clock.hs" "$XDG_CONFIG_HOME/argos/clock.30s.hs"
