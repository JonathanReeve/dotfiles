{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nixos.url = "nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware"; 
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened"; # Retained for both laptops
    caelestia-shell.url = "github:caelestia-dots/shell";
    caelestia-cli.url = "github:caelestia-dots/cli";
  };
  outputs = inputs @ { self,
              nixos, 
              nixos-hardware,
              home-manager, 
              ...
            }:
    {
      # Define configurations for both laptops with new names
      nixosConfigurations.fw12 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          ./hardware-configuration-fw12.nix  # Import specific hardware config
          nixos-hardware.nixosModules.framework-12-13th-gen-intel
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.jon = { pkgs, ... }: {
              imports = [ ./home.nix
                          ./hyprland.nix
                          inputs.nix-doom-emacs-unstraightened.homeModule
                          inputs.caelestia-shell.homeManagerModules.default
                        ];
            };
          }
        ];
      };

      nixosConfigurations.fw16 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          nixos-hardware.nixosModules.framework-16-7040-amd 
          ./hardware-configuration-fw16.nix  # Import specific hardware config
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.jon = { pkgs, ... }: {
              imports = [
                ./home.nix
                inputs.nix-doom-emacs-unstraightened.hmModule
              ];
            };
          }
        ];
      };
    };
}

