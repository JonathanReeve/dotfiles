{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    # nixos.url = "nixpkgs/nixos-unstable";
    nixos.url = "github:nixos/nixpkgs";
    # nixos.url = "/home/jon/Code/nixpkgs";
  };
  outputs = { self, nixos, home-manager, nix-doom-emacs }: {
     nixosConfigurations.jon-laptop = nixos.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./configuration.nix
                   home-manager.nixosModules.home-manager {
                     home-manager.useGlobalPkgs = true;
                     home-manager.useUserPackages = true;
                     home-manager.users.jon = { pkgs, ... }: {
                       imports = [ nix-doom-emacs.hmModule
                                   ./home.nix
                                 ];
                       programs.doom-emacs = {
                         enable = true;
                         # dependencyOverrides =
                         doomPrivateDir = ./emacs/doom.d;
                         extraPackages = with pkgs; [ mu pass gnupg ];
                         extraConfig = ''
                          (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
                          (setq epg-gpg-program "${pkgs.gnupg}/bin/gpg")
                        '';
                       };
                     };
                   }
                 ];
     };
  };
}
