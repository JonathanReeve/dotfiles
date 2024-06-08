{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      # url = "/home/jon/Code/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-straight = {
      url = "github:codingkoi/nix-straight.el?ref=codingkoi/apply-librephoenixs-fix";
      flake = false;
    };
    # nix-doom-emacs = {
    #   url = "github:nix-community/nix-doom-emacs";
    #   inputs = {
    #     nix-straight.follows = "nix-straight";
    #   };
    # };
    niri.url = "github:sodiboo/niri-flake";
    # Re-enable this after https://github.com/nix-community/nix-doom-emacs/issues/409 is fixed
    # nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
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
              nix-straight,
              # nix-doom-emacs,
              niri,
              stylix
            }:
    let overlays = [
      (final: prev: {mu = nixos-old.legacyPackages.${prev.system}.mu;})
    ];
    in {
    nixosConfigurations.jon-laptop = nixos.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./configuration.nix
                   # stylix.nixosModules.stylix
                   # ({...}: { nixpkgs.overlays = [ (import self.inputs.emacs-overlay) ];})
                   ({...}: { nixpkgs.overlays = overlays;})
                   # ./cachix.nix
                   nixos-hardware.nixosModules.framework-16-7040-amd

                   home-manager.nixosModules.home-manager {
                     home-manager.useGlobalPkgs = true;
                     home-manager.useUserPackages = true;
                     home-manager.backupFileExtension = "backup";
                     home-manager.users.jon = { pkgs, ... }: {
                       imports = [ ./home.nix
                                   niri.homeModules.niri
                                 #  ./nix-doom-emacs.nix
                                 # nix-doom-emacs.hmModule
                                 ];
                     };
                   }
                 ];
     };
  };
}
