{
  description = "My MacBook Pro Setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # darwin.url = "github:lnl7/nix-darwin";
    # Temporary fix: https://github.com/LnL7/nix-darwin/issues/933
    darwin.url = "github:nix-darwin/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Fix Mac Apps not appearing in Spotlight
    mac-app-util.url = "github:hraban/mac-app-util";
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
    # Manage homebrew with Nix
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    # Optional: Declarative tap management
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
  };

  outputs = { self
            , darwin, nixpkgs, home-manager
            , mac-app-util
            , nix-doom-emacs-unstraightened
            , nix-homebrew, homebrew-core, homebrew-cask }: {
    darwinConfigurations."Jonathans-MacBook-Pro" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [ ./configuration.nix
                  # ./copy-apps.nix
                  mac-app-util.darwinModules.default
                  home-manager.darwinModules.home-manager {
                    home-manager.useGlobalPkgs = true;
                    home-manager.useUserPackages = true;
                    home-manager.users.jon = import ./home.nix;
                    home-manager.sharedModules = [
                      mac-app-util.homeManagerModules.default
                      nix-doom-emacs-unstraightened.homeModule
                    ];
                  }
                  nix-homebrew.darwinModules.nix-homebrew
                  {
                    nix-homebrew = {
                      # Install Homebrew under the default prefix
                      enable = true;

                      # Apple Silicon Only: Also install Homebrew under the default Intel prefix for Rosetta 2
                      enableRosetta = true;

                      # User owning the Homebrew prefix
                      user = "jon";

                      # Automatically migrate existing Homebrew installations
                      autoMigrate = true;
                    };
                  }
                ];
    };
  };
}
