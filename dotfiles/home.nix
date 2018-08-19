let
  dots = "/home/jon/Dotfiles/dotfiles";
  scripts = "/home/jon/Dotfiles/scripts";
in
{
  # Have home-manager manage itself. 
  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/release-18.03.tar.gz";

  programs.git = {
    enable = true;
    userName = "Jonathan Reeve";
    userEmail = "jon.reeve@gmail.com";
  };
  programs.vim = {
	  enable = true;
	  plugins = [ "vim-airline" ];
	  settings = { ignorecase = true; };
	  extraConfig = ''
		  set mouse=a
      nnoremap n j
      nnoremap e k
      nnoremap i l
      nnoremap l i
		  '';
  };
  #xsession = {
    # enable = true;
    # pointerCursor.name = "breeze";
    # pointerCursor.size = 32;
    # windowManager.command = "bspwm";
  #};
  # xresources.properties = {
  #   Xft.antialias = 1;
  #   Xft.autohint = 0;
  #   Xft.dpi = 192;
  #   Xft.hinting = 1;
  #   Xft.hintstyle = "hintfull";
  #   Xft.lcdfilter = "lcddefault";
  # };
  # services.dunst.enable = true;

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
      "kcminputrc".text =
      ''
        [Mouse]
        cursorSize=48
        cursorTheme=breeze_cursors
      '';

      "konsolerc".text =
      ''
        [MainWindow]
        Height 1800=1196
        Height 900=743
        MenuBar=Disabled
        State=AAAA/wAAAAD9AAAAAAAAAoAAAAHgAAAABAAAAAQAAAAIAAAACPwAAAAA
        ToolBarsMovable=Disabled
        Width 1600=964
        Width 3200=1855
      '';

      "kdeglobals".text =
      ''
        [$Version]
        update_info=fonts_global.upd:Fonts_Global,fonts_global_toolbar.upd:Fonts_Global_Toolbar

        [General]
        fixed=Monospace,10,-1,5,50,0,0,0,0,0
        font=Sans Serif,10,-1,5,50,0,0,0,0,0
        menuFont=Sans Serif,10,-1,5,50,0,0,0,0,0
        smallestReadableFont=Sans Serif,8,-1,5,50,0,0,0,0,0
        toolBarFont=Sans Serif,10,-1,5,50,0,0,0,0,0

        [KScreen]
        ScaleFactor=2
        ScreenScaleFactors=eDP1=2;HDMI1=2;VIRTUAL1=2;

        [WM]
        activeFont=Noto Sans,12,-1,5,50,0,0,0,0,0,Bold
      '';
    };
  };
}
