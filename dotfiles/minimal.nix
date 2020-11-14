{pkgs, lib, ...}:

# This module describes an i3 config with a lot of other little programs
# that are typically used with it: rofi, termite, zathura, dunst, compton, etc.
# I will only sometimes use this config, mostly just using GNOME instead.
  
let
  # TODO: make these into options so I don't have to repeat myself?
  # Personal Inf
  name = "Jonathan Reeve";
  email = "jon.reeve@gmail.com";
  githubUsername = "JonathanReeve";
  # Paths
  dots = "/home/jon/Dotfiles/dotfiles";
  scripts = "/home/jon/Dotfiles/scripts";
  # Preferences
  font = "Fira Code";
  backgroundColor = "#243442"; # Blue steel
  foregroundColor = "#deedf9"; # Light blue
  warningColor = "#e23131"; # Reddish
  lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -p -t ''";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
in
{
  home = {
    packages = with pkgs; [
      #i3
      pywal
      ranger
      font-awesome_5 fira-code
      ];
    sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    sessionVariables.LC_ALL = "eo.UTF-8";
  };
  programs = {
    termite = {
      enable = true;
      clickableUrl = true;
      # backgroundColor = "\${xrdb:background}";
      backgroundColor = "rgba(32, 45, 56, 0.8)";
      foregroundColor = "\${xrdb:foreground}";
      font = "${font} 11";
    };
    rofi = {
      enable = true;
      theme = "~/.cache/wal/colors-rofi-dark.rasi";
      font = "${font} 11";
    };
    zathura = {
      enable = true;
      extraConfig = ''
        map n scroll down
        map e scroll up
        map h scroll left
        map i scroll right
        map j search next
        map J search previous
        set statusbar-v-padding 10
      '';
      options = {
        font = "${font} 11";
      };
    };
    };
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          geometry = "950x80-30+70";
          padding = 32;
          horizontal_padding = 30;
          # frame_width = 10;
          font = "${font} 10";
          line_height = 4;
          markup = "full";
          alignment = "left";
          word_wrap = "true";
          };
        shortcuts = {
          close = "ctrl+space";
          close_all = "ctrl+shift+space";
          history = "ctrl+grave";
          context = "ctrl+shift+period";
        };
        urgency_low = {
          timeout = 4;
          foreground = "${foregroundColor}";
          background = "${backgroundColor}";
        };
        urgency_normal = {
          timeout = 8;
          foreground = "${foregroundColor}";
          background = "${backgroundColor}";
        };
        urgency_critical = {
          timeout = 0;
          foreground = "${foregroundColor}";
          background = "${warningColor}";
          };
      };
    };
    picom = {
      enable = true;
      experimentalBackends = true;
      fade = true;
      blur = true;
      shadow = true;
      vSync = true;
    };
    polybar = {
      enable = true;
      package = pkgs.polybar.override {
        alsaSupport = true;
        githubSupport = true;
      };
      script = "polybar main_bar &";
      config = {
        "bar/main_bar" = {
           monitor = "eDP1";
           bottom = "false";
           height = 30;
           fixed-center = "true";
           background = "\${xrdb:background}";
           foreground = "\${xrdb:foreground}";
           line-size = 7;
           line-color = "\${xrdb:color4}";
           padding-right = "1%";
           module-margin-left = 1;
           module-margin-right = 1;
           font-0 = "${font}:size=11;1";
           font-1 = "FontAwesome:size=11:style=Solid;1";
           font-2 = "Noto Sans Symbols:size=11;1";
           modules-left = "bspwm xwindow";
           modules-center = "date";
           modules-right = "org-clock volume backlight filesystem memory cpu battery network";
        };
        "module/bspwm" = {
          type = "internal/bspwm";
          label-focused-underline = "\${xrdb:color4}";
          label-unfocused-underline = "\${xrdb:background}";
          label-focused = "%index%";
          label-unfocused = "%index%";
          label-urgent = "%index%";
          label-visible = "%index%";
          label-focused-padding = 1;
          label-unfocused-padding = 1;
          label-urgent-padding = 1;
          label-visible-padding = 1;
        };
        "module/date" = {
          type = "custom/script";
          # type = "internal/date";
          interval = 5;
          exec = "/run/current-system/sw/bin/date '+%a %d %b W%V-%u %R'";
          format-prefix-foreground = "\${xrdb:foreground}";
        };
        "module/battery" = {
           type = "internal/battery";
           battery = "BAT1";
           adapter = "ADP1";
           full-at = 96;
           format-charging = " <label-charging>";
           format-discharging = "<ramp-capacity> <label-discharging>";
           format-full = "";
           ramp-capacity-0 = "";
           ramp-capacity-1 = "";
           ramp-capacity-2 = "";
           ramp-capacity-3 = "";
           ramp-capacity-4 = "";
           ramp-capacity-foreground = "\${xrdb:foreground}";
        };
        "settings" = {screenchange-reload = "true";};
        "module/xwindow" = {
          type = "internal/xwindow";
          label = "%title:0:30:...%";
          label-padding = 10;
          label-foreground = "\${xrdb:color4}";
        };
        "module/network" = {
          type = "internal/network";
          interface = "wlp107s0";
          interval = "3.0";
          format-connected = "<label-connected>";
          label-connected = " %essid%";
        };
        "module/cpu" = {
          type = "internal/cpu";
          label = " %percentage:2%%";
        };
        "module/org-clock" = {
          type = "custom/script";
          interval =20;
          exec = "${scripts}/org-clock.hs 2> /dev/null";
          click-left = "emacsclient --eval '(org-clock-out)' && echo ' Stopped!'";
        };
        "module/memory" = {
          type = "internal/memory";
          label = " %percentage_used%%";
        };
        "module/filesystem" = {
          type = "internal/fs";
          mount-0 = "/";
          label-mounted = " %percentage_used%%";
        };
        "module/volume" = {
          type = "internal/alsa";
          master-soundcard = "hw:0";
          label-volume = " %percentage%";
          label-muted = "";
          click-left = "pactl set-sink-mute 1 toggle";
        };
        "module/backlight" = {
          type = "internal/backlight";
          card = "intel_backlight";
          format = "<ramp>";
          ramp-0 = "";
          ramp-1 = "";
          ramp-2 = "";
          ramp-3 = "";
          ramp-4 = "";
        };
      };
    };
    screen-locker = {
      enable = true;
      lockCmd = "${lockCmd}";
    };
    sxhkd = {
      enable = true;
      keybindings = {
        "super + Return" = "termite";
        "super + @space" = "env LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive rofi -show drun";
        # make sxhkd reload its configuration files
        "super + Escape" = "pkill -USR1 -x sxhkd";
        "super + shift + q" = "bspc quit";
        "super + {_,shift + }c" = "bspc node -{c,k}";
        # alternate between the tiled and monocle layout
        "super + m" = "bspc desktop -l next";
        # if the current node is automatic, send it to the last manual, otherwise pull the last leaf
        "super + y" = "bspc query -N -n focused.automatic && bspc node -n last.!automatic || bspc node last.leaf -n focused";
        # swap the current node and the biggest node
        "super + g" = "bspc node -s biggest";
        # set the window state
        "super + {q,w,f,p}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";
        # set the node flags
        "super + ctrl + {x,y,z}" = "bspc node -g {locked,sticky,private}";
        # focus the node in the given direction
        "super + {_,shift + }{h,n,e,i}" = "bspc node -{f,s} {west,south,north,east}";
        # focus the node for the given path jump
        "super + {l,u,y,;}" = "bspc node -f @{parent,brother,first,second}";
        # focus the next/previous node in the current desktop
        "alt + Tab" = "bspc node -f {next,prev}.local";
        # focus the next/previous desktop in the current monitor
        "super + bracket{left,right}" = "bspc desktop -f {prev,next}.local";
        # focus the last node/desktop
        "super + {grave,Tab}" = "bspc {node,desktop} -f last";
        # focus or send to the given desktop
        "super + {_,shift + }{a,r,s,t,d,6-9,0}" = "bspc {desktop -f,node -d} '^{1-9,10}'";
        # expand a window by moving one of its side outward
        "super + alt + {h,n,e,i}" = "bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";
        # contract a window by moving one of its side inward
        "super + alt + shift + {h,n,e,i}" = "bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";
          # move a floating window
        "super + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}";
        # move a floating window along the z axis
        "super + shift + {Down, Up}" = "bspc node -l {below, above}";
        # set desktop background
        "super + b" = "wal -gi ~/Bildoj/Ekranfonoj -o ~/Dotfiles/scripts/pywal-reload-everything.sh";
        "XF86MonBrightness{Up,Down}" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set {+10%,10%-}";
        "XF86Audio{Raise,Lower}Volume" = "exec ${pkgs.pulseaudio-ctl}/bin/pactl set-sink-volume 1 {+10%,-10%}";
        "XF86AudioMute" =  "exec ${pkgs.pulseaudio-ctl}/bin/pactl set-sink-mute 1 toggle";
        };
    };
  };
  xsession = {
    enable = true;
    scriptPath = ".xsession-hm";
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
    };
    windowManager.bspwm = {
      enable = true;
      settings = {
        "border_width" = 10;
        "window_gap" = 10;
        "focus_follows_pointer" = true;
        "normal_border_color" = "$color1";
        "active_border_color" = "$color2";
        "focused_border_color" = "$color15";
        "presel_feedback_color" = "$color1";
      };
      extraConfig = ''
      '';
      startupPrograms = [
        # Source pywal colors.
        " . /home/jon/.cache/wal/colors.sh"
        "xrdb -merge ~/.cache/wal/colors.Xresources"
        "${pkgs.pywal}/bin/wal -R"
        "polybar main_bar"
        "systemctl --user disable sxhkd"
        "systemctl --user stop sxhkd"
        "sxhkd -m 1"
      ];
      monitors = { eDP1 = [ "1" "2" "3" ];
                   DP1 = [ "4" "5" "6" ];
      };
    };
  };
}

