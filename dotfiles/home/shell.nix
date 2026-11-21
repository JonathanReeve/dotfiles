{ pkgs, vars, ... }:

{
  home.file.".inputrc".text = ''
    set editing-mode vi
    set keymap vi-command
  '';

  programs = {
    bottom.enable = true;
    broot.enable = true;
    direnv = {
      nix-direnv.enable = true;
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = true;
    };
    foot = {
      enable = true;
      server.enable = true;
    };
    git = {
      enable = true;
      settings = {
        user = {
          name = vars.name;
          email = vars.email;
        };
        pull.rebase = false;
      };
    };
    gnome-terminal = {
      profile.default = {
        allowBold = false;
        allowBell = false;
        font = vars.font;
        transparencyPercent = 90;
        themeVariant = "dark";
      };
    };
    neovim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [ ];
      vimAlias = true;
      extraConfig = ''
        set mouse=a
        nnoremap n j
        nnoremap j n
        nnoremap N J
        nnoremap J N
        nnoremap e k
        nnoremap k e
        nnoremap i l
        nnoremap l i
      '';
    };
    nix-index.enable = true;
    nushell = {
      enable = true;
      loadSessionVariables = true;
      configFile.source = ../config.nu;
      envFile.source = ../env.nu;
    };
    password-store = {
      enable = true;
      settings = { PASSWORD_STORE_DIR = "${vars.dokumentoj}/Personal/.password-store"; };
    };
    starship = {
      enable = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
    zsh = {
      enable = true;
      enableCompletion = true;
      enableVteIntegration = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
    };
    zoxide = {
      enable = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
  };
}
