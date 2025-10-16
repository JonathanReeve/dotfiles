{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    hyprpicker
    hyprpolkitagent
  ];
  wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true; # Xwayland can be disabled.
      settings = {
        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 2;
        };
        monitor =  "eDP-1,1920x1200@60,0x0,1";
        input = {
          kb_layout = "us";
          kb_variant = "colemak";
          kb_options = "caps:escape,esperanto:colemak";
          follow_mouse = 1;
        };

        decoration = {
          rounding = 10;
        };

        bind = [
          "Alt,space,exec,caelestia shell drawers toggle launcher"
          "SUPER,H,exec,alacritty"
          "SUPER_SHIFT,C,killactive,"
          "SUPER_SHIFT,Q,exit,"
          "SUPER_SHIFT,T,togglefloating,"
          "SUPER,space,exec,rofi -show drun"
          "SUPER,P,pseudo,"
          "SUPER,N,workspace, next"
          "SUPER,E,workspace, prev"
          "Alt,H,movefocus,l"
          "Alt,I,movefocus,r"
          "Alt,E,movefocus,u"
          "Alt,N,movefocus,d"
          "Alt_Shift,H,movewindoworgroup,l"
          "Alt_Shift,N,movewindoworgroup,d"
          "Alt_Shift,E,movewindoworgroup,u"
          "Alt_Shift,I,movewindoworgroup,r"
          "SUPER,J,workspace,1"
          "SUPER,L,workspace,2"
          "SUPER,U,workspace,3"
          "SUPER,Y,workspace,4"
          "SUPER,5,workspace,5"
          "SUPER,6,workspace,6"
          "SUPER,7,workspace,7"
          "SUPER,8,workspace,8"
          "SUPER,9,workspace,9"
          "SUPER,0,workspace,10"
          "SUPER_SHIFT,J,movetoworkspace,1"
          "SUPER_SHIFT,L,movetoworkspace,2"
          "SUPER_SHIFT,U,movetoworkspace,3"
          "SUPER_SHIFT,Y,movetoworkspace,4"
          "ALT,5,movetoworkspace,5"
          "ALT,6,movetoworkspace,6"
          "ALT,7,movetoworkspace,7"
          "ALT,8,movetoworkspace,8"
          "ALT,9,movetoworkspace,9"
          "ALT,0,movetoworkspace,10"
	  ];
    bindm = [
      "SUPER, mouse:272, movewindow" # Super + Left mouse button moves windows
      "SUPER, mouse:273, resizewindow" # Super + Right mouse button resizes
    ];
    exec-once = [
      "waybar"
    ];
    gesture = [
      "3, left, workspace"
      "3, right, workspace"
    ];
      };
  };
  programs = {
    caelestia = {
      enable = true;
      cli.enable = true; # optional, for CLI tools
      settings = {
        paths.wallpaperDir = "~/Bildoj/Ekranfonoj";
      };
      extraConfig = ''
      { "theme": "dark" }
    '';
    };
    hyprlock.enable = true;
  };
  services = {
    hypridle.enable = true;
    hyprsunset.enable = true;
  };
}
