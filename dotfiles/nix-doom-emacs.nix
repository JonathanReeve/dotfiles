{ config, lib, pkgs, nixos, ... }:

{
  programs.doom-emacs = {
    enable = true;
    # dependencyOverrides =
    doomPrivateDir = ./emacs/doom.d;
    emacsPackagesOverlay = self: super: {
      org-roam = super.org-roam.overrideAttrs (esuper: {
        src = pkgs.fetchFromGitHub {
          owner = "org-roam";
          repo = "org-roam";
          rev = "e9ae19c01cb1fac8256e404b3f9c06f4be5468e6";
          sha256 = "4U75arstVrPA9mXNd6c4Jmjn1uEgFXPk7DhZo08v9Dg=";
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
    };
    extraPackages = with pkgs; [ mu pass gnupg ];
    extraConfig = ''
                          (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
                          (setq epg-gpg-program "${pkgs.gnupg}/bin/gpg")
                        '';
  };
}
