{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-doom-emacs.url = "github:JonathanReeve/nix-doom-emacs";
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
                           org-roam = super.org-roam.overrideAttrs (esuper: {
                             src = pkgs.fetchFromGitHub {
                               owner = "org-roam";
                               repo = "org-roam";
                               rev = "5dce6261a2a45798cdf0c65371b76c83dd5b1de6";
                               sha256 = "4U75vYoaVrPA9mXNd6c4Jmjn1uEgFXPk7DhZo08v9Dg=";
                             };
                           });
                           org-roam-bibtex = super.org-roam-bibtex.overrideAttrs (esuper: {
                             src = pkgs.fetchFromGitHub {
                               owner = "org-roam";
                               repo = "org-roam-bibtex";
                               rev = "c13a05b2c855ba1516241d8a1de33bf2c689d6e4";
                               sha256 = "PbF/oxaFDq1rc9g3yz2cwLJAyanNfBwxKTxWbhjINVQ=";
                             };
                           });
                           org-roam-ui = pkgs.stdenv.mkDerivation {
                             pname = "org-roam-ui";
                             version = "2020-08-14";
                             buildInputs = [ pkgs.emacs ];
                             src = pkgs.fetchFromGitHub {
                               owner = "org-roam";
                               repo = "org-roam-ui";
                               rev = "b153f4fee99e36dec0fb56d987026d53bf97a0e8";
                               sha256 = "arstoxaFDq1rc9g3yz2cwLJAyanNfBwxKTxWbhjINVQ=";
                             };
                             buildPhase = ''
                                runHook preBuild
                                emacs -L . --batch -f batch-byte-compile *.el
                                runHook postBuild
                              '';
                             installPhase = ''
                                runHook preInstall
                                install -d $out/share/emacs/site-lisp
                                install *.el *.elc $out/share/emacs/site-lisp
                                runHook postInstall
                              '';
                             meta = {
                               description = "Visualizations for Org-Roam";
                               homepage = "https://github.com/org-roam/org-roam-ui";
                               license = nixos.lib.licenses.gpl3Plus;
                               maintainers = with nixos.lib.maintainers; [ JonathanReeve ];
                             };
                           };
                           vulpea = super.vulpea.overrideAttrs (esuper: {
                             src = pkgs.fetchFromGitHub {
                               owner = "d12frosted";
                               repo = "vulpea";
                               rev = "ebbfc00978603c40222e260f06dbec9d781e3306";
                               sha256 = "Bxs2op8mX3XsnYLTwBVr/4yaoVHFqnLMJRXWk0GBqL8=";
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
