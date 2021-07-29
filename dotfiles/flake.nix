{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nixos.url = "nixpkgs/nixos-unstable";
    # nixos.url = "github:nixos/nixpkgs";
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
       emacsPackagesOverlay = self: super: {
         # Use experimental versions of org-roam and org-roam-bibtex.
         # I wish I could just do this in my doom config, but nix-straight.el doesn't seem to respect recipes.
         org-roam = super.org-roam.overrideAttrs (esuper: {
            src = pkgs.fetchFromGitHub {
              owner = "org-roam";
              repo = "org-roam";
              rev = "e997c017deab234a0a067914d7bb6e81e3fa9d88";
              sha256 = "Jf36HnGVtwDhrV7542isfmncWuZ6IV/v4lYfPwLzfSs=";
              };
            });
         org-roam-bibtex = super.org-roam-bibtex.overrideAttrs (esuper: {
            src = pkgs.fetchFromGitHub {
              owner = "org-roam";
              repo = "org-roam-bibtex";
              rev = "c9865196efe7cfdfcced0d47ea3e5b39bdddd162";
              sha256 = "z9ORXNMiFDdW1GsdQIIhWDCNO5NiTXJqGv+pFbI5PjE=";
              };
            });
         vulpea = super.vulpea.overrideAttrs (esuper: {
           src = pkgs.fetchFromGitHub {
             owner = "d12frosted";
             repo = "vulpea";
             rev = "a053350af4f43705908bfbd862824bf01e6b45cd";
             sha256 = "DOccj9qr1sQAKWXNcFds1Y29khgYbxpUdzoMx0oND4E=";
           };
         });
         };
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
