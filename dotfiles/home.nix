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
    # Disabling this for the moment, since home-manager seems to think
    # this program doesn't exist.
    #  fish = {
    #    enable = true;
    #    shellAbbrs = {
    #      # Git abbreviations
    #      "ga" = "git add";
    #      "gc" = "git commit";
    #      "gcam" = "git commit -am";
    #      "gcm" = "git commit -m";
    #      "gco" = "git checkout";
    #      "gcob" = "git checkout -b";
    #      "gcom" = "git checkout master";
    #      "gcod" = "git checkout develop";
    #      "gd" = "git diff";
    #      "gp" = "git push";
    #      "gdc" = "git diff --cached";
    #      "glg" = "git lg";
    #      "gst" = "git status";
    #      # Other abbreviations
    #      "em" = "emacsclient -c";
    #      "pw" = "vim ~/Dropbox/Personal/.p10.txt";
    #    };
    # };
    rofi = {
      enable = true;
      font = "Hack 20";
      theme = "~/.cache/wal/colors-rofi-dark.rasi";
    };
    termite = {
      enable = true;
      font = "Hack 11";
    };
  };

  xsession = {
    enable = true;
    pointerCursor = {
        package = pkgs.vanilla-dmz;
        name = "Vanilla-DMZ";
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
    "Xcursor.size" = 48;
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
      "qutebrowser/userscripts/" = {
        source = "${scripts}/qutebrowser-userscripts";
        recursive = true;
      };
    };
    configFile = {
      "fish/config.fish".source = "${dots}/config.fish";
      "qutebrowser/config.py".source = "${dots}/qutebrowser.py";

      # BSPWM stuff
      "polybar/config".source = "${dots}/polybar";
      "sxhkd/sxhkdrc".source = "${dots}/sxhkdrc";
      "bspwm/bspwmrc".source = "${dots}/bspwmrc";
    };
  };
}
