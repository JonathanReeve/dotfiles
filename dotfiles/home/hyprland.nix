{ pkgs, vars, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    configType = "hyprlang";
    plugins = [
      # inputs.hyprgrass.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];
    settings = {
      source = [ "~/.config/hypr/dms/colors.conf" ];
      general = {
        gaps_in = 5;
        gaps_out = 20;
        border_size = 5;
        "col.active_border" = "$primary";
        "col.inactive_border" = "$outline";
        layout = "dwindle";
      };
      scrolling = {
        column_width = 0.5;
        focus_fit_method = 1;
      };
      monitor = ",preferred,auto,1";
      input = {
        kb_layout = "us";
        kb_variant = "colemak";
        kb_options = "caps:escape,esperanto:colemak";
        follow_mouse = 1;
        touchpad = {
          natural_scroll = true;
          tap-to-click = true;
        };
      };
      decoration = {
        rounding = 10;
        blur = {
          enabled = true;
          size = 3;
          passes = 2;
        };
      };
      windowrule = [
        "float on, center on, size 1000 700, match:title ^(vulpea-find)$"
        "float on, center on, match:class ^(MEGAsync|megasync)$"
      ];
      debug = {
        vfr = true;
      };
      misc = {
        disable_hyprland_logo = true;
        force_default_wallpaper = 0;
      };
      animations = {
        enabled = true;
        bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
        animation = [
          "windows, 1, 7, myBezier"
          "windowsOut, 1, 7, default, popin 80%"
          "border, 1, 10, default"
          "borderangle, 1, 8, default"
          "fade, 1, 7, default"
          "workspaces, 1, 6, default, slidevert"
        ];
      };
      workspace = [
        "1, persistent:true"
        "2, persistent:true"
        "3, persistent:true"
        "4, persistent:true"
      ];
      bind = [
        "SUPER,H,exec,footclient"
        "SUPER_SHIFT,C,killactive,"
        "SUPER_SHIFT,Q,exec,dms ipc powermenu open"
        "SUPER_SHIFT,T,togglefloating,"
        "SUPER,space,exec,dms ipc launcher toggle"
        "SUPER,P,pseudo,"
        "SUPER,N,workspace, e+1"
        "SUPER,E,workspace, e-1"
        "SUPER,F,fullscreen, 0"
        "SUPER,T,exec,${vars.scripts}/hypr.nu float-toggle"
        "SUPER,G,exec,${vars.scripts}/hypr.nu gaps-toggle"
        "SUPER,S,exec,${vars.scripts}/hypr.nu layout-toggle"
        "SUPER,W,exec,dms ipc wallpaper set $(find ~/Bildoj/Ekranfonoj -maxdepth 1 -type f | shuf -n 1)"
        "SUPER_SHIFT,W,exec,dms ipc wallpaper set $(find /run/media/jon/systemrestore/.systemrestore/Bildoj -maxdepth 1 -type f | shuf -n 1)"
        "SUPER_SHIFT,E,exec,${vars.emacsclient} -c -a emacs"
        "Alt,H,movefocus,l"
        "Alt,I,movefocus,r"
        "Alt,E,movefocus,u"
        "Alt,N,movefocus,d"
        "SUPER_SHIFT,H,movewindow,l"
        "SUPER_SHIFT,I,movewindow,r"
        "SUPER_SHIFT,E,movewindow,u"
        "SUPER_SHIFT,N,movewindow,d"
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
        ",XF86AudioRaiseVolume,exec,dms ipc call audio increment 5"
        ",XF86AudioLowerVolume,exec,dms ipc call audio decrement 5"
        ",XF86AudioMute,exec,dms ipc call audio mute"
        ",XF86AudioPlay,exec,dms ipc call mpris playPause"
        ",XF86AudioPrev,exec,dms ipc call mpris previous"
        ",XF86AudioNext,exec,dms ipc call mpris next"
        ",XF86MonBrightnessUp,exec,dms ipc call brightness increment 10 \"\""
        ",XF86MonBrightnessDown,exec,dms ipc call brightness decrement 10 \"\""
        ",Print,exec,${vars.emacsclient} -c -F '((name . \"vulpea-find\"))' --eval '(vulpea-find)'"
        "SUPER_SHIFT,V,exec,python3 ${vars.scripts}/visualize_habits.py && qutebrowser journal_stats.html habit_heatmap.html"
      ];
      bindm = [
        "SUPER, mouse:272, movewindow"
        "SUPER, mouse:273, resizewindow"
      ];
      exec-once = [
        "sleep 3 && megasync"
        "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
      ];
      gestures = {
        workspace_swipe_touch = false; # Set to false to allow hyprgrass to work
        gesture = [
          "3, vertical, workspace"
          "3, left, dispatcher, layoutmsg, move +col"
          "3, right, dispatcher, layoutmsg, move -col"
        ];
      };
      plugin = {
        touch_gestures = {
          # The number of fingers for a workspace swipe
          workspace_swipe_fingers = 3;
          sensitivity = 4.0;
          experimental = {
            send_cancel = true;
          };

          # Use both bind and gesture for maximum compatibility
          hyprgrass-bind = [
            ", swipe:3:u, workspace, e-1"
            ", swipe:3:d, workspace, e+1"
            ", swipe:3:l, layoutmsg, move +col"
            ", swipe:3:r, layoutmsg, move -col"
            ", edge:d:u, exec, dms ipc launcher toggle"
          ];
          hyprgrass-gesture = [
            "swipe, 3, vertical, workspace"
            "swipe, 3, left, dispatcher, layoutmsg, move +col"
            "swipe, 3, right, dispatcher, layoutmsg, move -col"
          ];
        };
      };
    };
  };
}
