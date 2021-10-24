{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-doom-emacs.url = "github:JonathanReeve/nix-doom-emacs";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    nixos.url = "nixpkgs/nixos-unstable";
    # nixos.url = "github:nixos/nixpkgs";
    # nixos.url = "/home/jon/Code/nixpkgs";
  };
  outputs = { self, nixos, nixos-hardware, home-manager, nix-doom-emacs }: {
     nixosConfigurations.jon-laptop = nixos.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./configuration.nix
                   # TODO. This makes the kernel rebuild, apparently
                   # nixos-hardware.nixosModules.dell-xps-13-9310
                   home-manager.nixosModules.home-manager {
                     home-manager.useGlobalPkgs = true;
                     home-manager.useUserPackages = true;
                     home-manager.users.jon = { pkgs, ... }: {
                       imports = [ ./home.nix
                                   # ./nix-doom-emacs.nix
                                   # nix-doom-emacs.hmModule
                                 ];
                     };
                   }
                 ];
     };
  };
}
