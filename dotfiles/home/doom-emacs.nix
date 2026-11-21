{ pkgs, inputs, ... }:

{
  imports = [
    inputs.nix-doom-emacs-unstraightened.homeModule
  ];

  programs.doom-emacs = {
    enable = true;
    doomDir = ../doom;
    extraBinPackages = with pkgs; [ fd ripgrep git ];
    extraPackages = epkgs: with pkgs; [
      pass gnupg (aspellWithDicts (dicts: with dicts; [ en eo fr ]))
      ripgrep epkgs.treesit-grammars.with-all-grammars
    ];
    emacsPackageOverrides = eself: esuper: {
      org-todoist = esuper.org-todoist.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace org-todoist.el \
            --replace-fail $';;;###autoload\n(transient-define-prefix org-todoist-dispatch' \
                           $';;;###autoload (autoload \'org-todoist-dispatch "org-todoist" nil t)\n(transient-define-prefix org-todoist-dispatch'
        '';
      });
      typst-ts-mode = esuper.typst-ts-mode.overrideAttrs (old: {
        postInstall = (old.postInstall or "") + ''
          sed -i "1i (require 'compile)" $out/share/emacs/site-lisp/elpa/typst-ts-mode-*/typst-ts-mode-autoloads.el
        '';
      });
    };
  };
}
