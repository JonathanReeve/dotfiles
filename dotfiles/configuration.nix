{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    ];
  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.fira-code
      pkgs.roboto
    ];
  };
  homebrew = {
    enable = false;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
    };
    brews = [
      "maven"
      "gradle"
      # "openjdk"
      "npm"
      # "gcc"
      # "libgccjit"
      # "emacs-plus"
      # "zotero"
      "docker"
    ];
    casks = [
      "utm"
      "miniconda"
      # "emacsclient"
      # "google-chrome"
    ];
    taps = [ # "railwaycat/emacsmacport" 
             # "d12frosted/emacs-plus"
    ]; 
  };
  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/Dotfiles/dotfiles/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix = {
    package = pkgs.nixFlakes;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

  nixpkgs.config.allowUnfree = true;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  users.users.jon = { 
    name = "jon";
    home = "/Users/jon";
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
