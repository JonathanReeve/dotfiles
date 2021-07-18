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
              owner = "jethrokuan";
              repo = "org-roam";
              rev = "6285561c4ff7254bbeead8908422cd4263786c6f";
              sha256 = "iicx/zsZIk/5ivy5xn3HW03JBCO4ascVdgc5qzu3yww=";
              };
            });
         org-roam-bibtex = super.org-roam-bibtex.overrideAttrs (esuper: {
            src = pkgs.fetchFromGitHub {
              owner = "org-roam";
              repo = "org-roam-bibtex";
              rev = "c4d3afcb3143cd7db30589c81110f677510c1bf9";
              sha256 = "iicx/zsZIk/5ivy5xn3HW03JBCO4ascVdgc5qzu3yww=";
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
