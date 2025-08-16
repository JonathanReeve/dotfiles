{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      # url = "/home/jon/Code/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    niri.url = "github:sodiboo/niri-flake";
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
    nixos.url = "nixpkgs/nixos-unstable";
    # nixos.url = "/home/jon/Code/nixpkgs";
    # emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-old.url = "github:nixos/nixpkgs/nixos-23.11";
    stylix.url = "github:danth/stylix";

  };
  outputs = { self, 
              nixos, 
              nixos-hardware,
              nixos-old,
              home-manager, 
              nix-doom-emacs-unstraightened,
              niri,
              stylix
            }:
    { nixosConfigurations.jon-laptop = nixos.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./configuration.nix
                   nixos-hardware.nixosModules.framework-16-7040-amd
                   home-manager.nixosModules.home-manager {
                     home-manager.useGlobalPkgs = true;
                     home-manager.useUserPackages = true;
                     home-manager.backupFileExtension = "backup";
                     home-manager.users.jon = { pkgs, ... }: {
                       imports = [ ./home.nix
                                   niri.homeModules.niri
                                   nix-doom-emacs-unstraightened.hmModule
                                 ];
                     };
                   }
                 ];
     };
  };
}
