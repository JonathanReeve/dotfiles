{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      # url = "/home/jon/Code/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    caelestia-shell.url = "github:caelestia-dots/shell";
    caelestia-cli.url = "github:caelestia-dots/cli";
    # niri.url = "github:sodiboo/niri-flake";
    # nixos-hardware.url = github:NixOS/nixos-hardware/master;
   nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
   nixos.url = "nixpkgs/nixos-unstable";
  };
  outputs = inputs @ { self, nixos, home-manager, ... }:
    { nixosConfigurations.jon-laptop = nixos.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./configuration.nix
                   # ./cachix.nix
                   # TODO. This makes the kernel rebuild, apparently
                   # nixos-hardware.nixosModules.dell-xps-13-9310
                   # nixos-hardware.nixosModules.common-cpu-intel
                   # nixos-hardware.nixosModules.common-pc-laptop
                   # nixos-hardware.nixosModules.common-pc-laptop-ssd

                   home-manager.nixosModules.home-manager {
                     home-manager.useGlobalPkgs = true;
                     home-manager.useUserPackages = true;
                     home-manager.backupFileExtension = "backup";
                     home-manager.extraSpecialArgs = { inherit inputs; };
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
  };
}
