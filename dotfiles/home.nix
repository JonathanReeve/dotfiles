let
  dots = "/home/jon/dotfiles/dotfiles";
in
{
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
		  '';
  };
  # xsession = {
  #   enable = true;
  #     #pointerCursor.size =   32;
  #   windowManager.command = "bspwm";
  # };
  # xresources.properties = {
  #   Xft.antialias = 1;
  #   Xft.autohint = 0;
  #   Xft.dpi = 192;
  #   Xft.hinting = 1;
  #   Xft.hintstyle = "hintfull";
  #   Xft.lcdfilter = "lcddefault";
  # };
  # services.dunst.enable = true;
  home.file = {
    ".spacemacs".source = "${dots}/spacemacs";

    ".inputrc".text =
    ''
      set editing-mode vi
      set keymap vi-command
    '';

    ".local/share/qutebrowser/userscripts/org-link".text =
    ''
      #!/bin/bash
      emacsclient "org-protocol://capture?template=l&url=$QUTE_URL&title=$QUTE_TITLE&body=$QUTE_SELECTED_TEXT"
    '';

  };
  xdg = {
    enable = true;
    configFile = {
      "polybar/config".source = "${dots}/polybar";
      "fish/config.fish".source = "${dots}/config.fish";
      "qutebrowser/config.py".source = "${dots}/qutebrowser.py";
      "sxhkd/sxhkdrc".source = "${dots}/sxhkdrc";

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

      "bspwm/bspwmrc".text =
      ''

      '';
    };
  };
}
