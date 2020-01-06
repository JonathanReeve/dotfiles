# Dotfiles

My personal config files for NixOS, Spacemacs, and so on. (Jonathan Reeve: jon.reeve@gmail.com). 

Don't use these unless you're me, or you really know what you're doing.


# Installation

``` sh
# Run this on NixOS. 

# First, create symlinks for nix files
fish scripts/setup.fish 

# Have home-manager handle the rest. 
# First install home-manager:

nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-shell '<home-manager>' -A install


home-manager switch
```
