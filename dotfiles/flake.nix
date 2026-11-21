{
  description = "My NixOS configuration.";
  inputs = {
    nixos.url = "nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
    dms = {
      url = "github:duxlabs-dev/dux-shell";
      inputs.nixpkgs.follows = "nixos";
    };
    # Local copy for testing
    # dms = {
    #   url = "git+file:///home/jon/Programaroj/DankMaterialShell?ref=eo-traduko";
    #   inputs.nixpkgs.follows = "nixos";
    # };
    dms-plugin-registry = {
      url = "github:duxlabs-dev/dux-plugin-registry";
      inputs.nixpkgs.follows = "nixos";
    };
    danksearch = {
      url = "github:duxlabs-dev/dux-search";
      inputs.nixpkgs.follows = "nixos";
    };
    iio-hyprland.url = "github:JeanSchoeller/iio-hyprland";
    hyprland.url = "github:hyprwm/Hyprland";
    hyprgrass = {
      url = "github:horriblename/hyprgrass";
      inputs.hyprland.follows = "hyprland";
    };
    antigravity-nix = {
      url = "github:jacopone/antigravity-nix";
      inputs.nixpkgs.follows = "nixos";
    };
  };

  outputs = inputs@{ self, nixos, nixos-hardware, home-manager, ... }:
    let
      mkHost = { hostName, extraModules ? [ ] }:
        nixos.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./configuration.nix
            ./hosts/${hostName}.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                backupFileExtension = "backup";
                extraSpecialArgs = { inherit inputs; };
                users.jon = ./home.nix;
              };
            }
          ] ++ extraModules;
        };
    in
    {
      nixosConfigurations = {
        fw12 = mkHost {
          hostName = "fw12";
          extraModules = [
            nixos-hardware.nixosModules.framework-12-13th-gen-intel
          ];
        };

        fw16 = mkHost {
          hostName = "fw16";
          extraModules = [
            nixos-hardware.nixosModules.framework-16-7040-amd
          ];
        };
      };
    };
}
