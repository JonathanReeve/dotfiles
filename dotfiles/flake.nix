{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    knock.url = "github:BentonEdmondson/knock";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    nixos.url = "nixpkgs/nixos-unstable";
    # jupyterWith.url = "github:tweag/jupyterWith";
    # nixos.url = "github:nixos/nixpkgs";
    # nixos.url = "/home/jon/Code/nixpkgs";
  };
  outputs = { self, knock, nixos, nixos-hardware, home-manager, nix-doom-emacs }: {
     nixosConfigurations.jon-laptop = nixos.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./configuration.nix
                   # ./cachix.nix
                   # TODO. This makes the kernel rebuild, apparently
                   # nixos-hardware.nixosModules.dell-xps-13-9310
                   nixos-hardware.nixosModules.common-cpu-intel
                   nixos-hardware.nixosModules.common-pc-laptop
                   nixos-hardware.nixosModules.common-pc-laptop-ssd

                   home-manager.nixosModules.home-manager {
                     home-manager.useGlobalPkgs = true;
                     home-manager.useUserPackages = true;
                     home-manager.users.jon = { pkgs, ... }: {
                       imports = [ ./home.nix
                                 #  ./nix-doom-emacs.nix
                                 #  nix-doom-emacs.hmModule
                                 ];
                       home.packages = [ knock.defaultPackage.x86_64-linux ];
                     };
                   }
                 ];
     };
  };
}
