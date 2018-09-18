# Dotfiles

My personal dotfiles. Configurations for NixOS, Spacemacs, and other utilities.

## Setup

Don't try this on your own machine, unless you're me. 

```sh
ln -s $PWD/home.nix $XDG_CONFIG_HOME/nixpkgs/home.nix
sudo ln -s $PWD/configuration.nix /etc/nix/configuration.nix
sudo ln -s $PWD/hardware-configuration.nix /etc/nix/hardware-configuration.nix

git clone https://github.com/rycee/home-manager.git ~/Code/home-manager
home-manager switch
```
