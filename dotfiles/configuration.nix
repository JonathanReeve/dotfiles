{ config, pkgs, lib, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    ];
  fonts = {
    packages = [
      pkgs.cascadia-code
      pkgs.roboto
      pkgs.iosevka-comfy.comfy
      pkgs.agave
      pkgs.nerd-fonts.victor-mono
      pkgs.nerd-fonts.sauce-code-pro
      pkgs.nerd-fonts.monofur
      pkgs.nerd-fonts.fantasque-sans-mono
      pkgs.nerd-fonts.fira-code
      pkgs.nerd-fonts.departure-mono
    ];
  };
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
    };
    brews = [
      # "maven"
      # "gradle"
      # "openjdk"
      "npm"
      # "gcc"
      # "libgccjit"
      # "emacs-plus"
      # "zotero"
      # "docker"
      # "elixir"
      "libvterm"
      # "plover"
      "awscli"
    ];
    casks = [
      "utm"
      "miniconda"
      "applejdk-17"
      # "hammerspoon"
      # "logseq"
      # "nikitabobko/tap/aerospace"
      # "emacsclient"
      # "google-chrome"
    ];
    taps = [
      "apple/applejdk"
      # "railwaycat/emacsmacport"
             # "d12frosted/emacs-plus"
             # "nikitabobko"
    ];
  };
  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/Dotfiles/dotfiles/configuration.nix";

  # Determinate Nix requires nix-darwin's management of the nix install to be disabled
  nix.enable = false;

  nixpkgs.config.allowUnfree = true;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs = {
    zsh.enable = true;  # default shell on catalina
  };
  # programs.fish.enable = true;

  users.users.jon = { 
    name = "jon";
    home = "/Users/jon";
  };

  system.primaryUser = "jon";
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

}
