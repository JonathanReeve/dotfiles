{pkgs, ...}:

let
  dots = "/home/jon/Dotfiles/dotfiles";
  scripts = "/home/jon/Dotfiles/scripts";
in
{
  programs = {
    # Have home-manager manage itself. 
    home-manager = {
      enable = true;
      path = "https://github.com/rycee/home-manager/archive/release-18.03.tar.gz";
    };
    git = {
      enable = true;
      userName = "Jonathan Reeve";
      userEmail = "jon.reeve@gmail.com";
    };
    vim = {
      enable = true;
      plugins = [ "vim-airline" ];
      settings = { ignorecase = true; };
      extraConfig =
      ''
        set mouse=a
        nnoremap n j
        nnoremap e k
        nnoremap i l
        nnoremap l i
      '';
    };
    rofi = {
      enable = true;
      font = "Hack 20";
    };
    termite = {
      enable = true;
      font = "Hack 12";
    };
  };

  xsession = {
    enable = true;
    pointerCursor = {
        package = pkgs.paper-icon-theme;
        name = "Paper";
        size = 64;
    };
    windowManager.command = "bspwm";
  };

  xresources.properties = {
    "Xft.antialias" = 1;
    "Xft.autohint" = 0;
    "Xft.dpi" = 192;
    "Xft.hinting" = 1;
    "Xft.hintstyle" = "hintfull";
    "Xft.lcdfilter" = "lcddefault";
    "Xcursor.theme" = "breeze_cursors";
    "Xcursor.size" = 64;
  };

  gtk = {
    enable = true;
    theme = {
      package = pkgs.arc-theme;
      name = "Arc-Dark";
    };
    iconTheme = {
      package = pkgs.paper-icon-theme;
      name = "Paper";
    };
  };

  qt = {
    enable = true;
    useGtkTheme = true;
  };

  services = {
    dunst.enable = true;
    compton = {
      enable = true;
      blur = true;
    };
  };

  # Dotfiles for the home root, ~/
  home.file = {
    ".spacemacs".source = "${dots}/spacemacs";

    # Vim all the things!
    ".inputrc".text =
    ''
      set editing-mode vi
      set keymap vi-command
    '';
  };

  # Dotfiles for ~/.config, ~/.local/share, etc. 
  xdg = {
    enable = true;
    dataFile = {
      "qutebrowser/userscripts/password_fill".source = "${scripts}/password_fill";
      "qutebrowser/userscripts/dict".source = "${scripts}/dict";
      "qutebrowser/userscripts/org-link".source = "${scripts}/org-link";
    };
    configFile = {
      "fish/config.fish".source = "${dots}/config.fish";
      "qutebrowser/config.py".source = "${dots}/qutebrowser.py";

      # BSPWM stuff
      "polybar/config".source = "${dots}/polybar";
      "sxhkd/sxhkdrc".source = "${dots}/sxhkdrc";
      "bspwm/bspwmrc".source = "${dots}/bspwmrc";

      # Plasma configs
      # Disabling these for now. 

      # "plasma-workspace/env/set_window_manager.sh" = {
      #   text =
      #   ''
      #     #!/run/current-system/sw/bin/bash
      #     export KDEWM=/run/current-system/sw/bin/bspwm
      #   '';
      #   executable = true;
      # };

      # "kcminputrc".text =
      # ''
      #   [Mouse]
      #   cursorSize=48
      #   cursorTheme=breeze_cursors
      # '';

      # "konsolerc".text =
      # ''
      #   [MainWindow]
      #   Height 1800=1196
      #   Height 900=743
      #   MenuBar=Disabled
      #   State=AAAA/wAAAAD9AAAAAAAAAoAAAAHgAAAABAAAAAQAAAAIAAAACPwAAAAA
      #   ToolBarsMovable=Disabled
      #   Width 1600=964
      #   Width 3200=1855
      # '';

      # "kdeglobals".text =
      # ''
      #   [$Version]
      #   update_info=fonts_global.upd:Fonts_Global,fonts_global_toolbar.upd:Fonts_Global_Toolbar

      #   [General]
      #   fixed=Monospace,10,-1,5,50,0,0,0,0,0
      #   font=Sans Serif,10,-1,5,50,0,0,0,0,0
      #   menuFont=Sans Serif,10,-1,5,50,0,0,0,0,0
      #   smallestReadableFont=Sans Serif,8,-1,5,50,0,0,0,0,0
      #   toolBarFont=Sans Serif,10,-1,5,50,0,0,0,0,0

      #   [KScreen]
      #   ScaleFactor=2
      #   ScreenScaleFactors=eDP1=2;HDMI1=2;VIRTUAL1=2;

      #   [WM]
      #   activeFont=Cousine,11,-1,5,50,0,0,0,0,0,Regular
      # '';
    };
  };
}
