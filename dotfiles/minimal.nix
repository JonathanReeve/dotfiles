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
      enable = false;
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
      fadeDelta = 5;
      blur = true;
      shadow = true;
      vSync = true;
    };
    polybar = {
      enable = true;
      package = pkgs.polybarFull;
      script = "/usr/bin/env polybar main_bar &";
      config = {
        "bar/main_bar" = {
           monitor = "eDP-1";
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
           font-1 = "Font Awesome 5 Free:size=11:style=Solid;1";
           font-2 = "Noto Sans Symbols:size=11;1";
           modules-left = "i3 xwindow";
           modules-center = "date";
           modules-right = "org-clock volume backlight filesystem memory cpu battery network";
        };
        "module/i3" = {
          type = "internal/i3";
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
           battery = "BAT0";
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
          interface = "wlp0s20f3";
          interval = "3.0";
          format-connected = "<label-connected>";
          label-connected = "";
        };
        "module/cpu" = {
          type = "internal/cpu";
          label = " %percentage:2%%";
        };
        "module/org-clock" = {
          type = "custom/script";
          interval =20;
          exec = "/run/current-system/sw/bin/runhaskell ${scripts}/org-clock.hs 2> /dev/null";
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
  };
  systemd.user = {
    services = {
      dwall = {
        Unit = {
          Description = "Set dynamic wallpaper using Dwall";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${scripts}/dynamic-wallpaper/dwall.sh -p -s beach";
        };
        Install = {
          WantedBy = ["multi-user.target"];
        };
      };
    };
    timers = {
      dwall = {
        Unit = {
          Description = "Set dynamic wallpaper using Dwall.";
          Requires = "dwall.service";
        };
        Timer = {
          Unit = "dwall.service";
          OnCalendar="*-*-* *:00:00"; # Every hour
        };
        Install = {
          WantedBy = ["timers.target"];
        };
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
    windowManager.i3 = {
        enable = true;
        extraConfig = ''
          set_from_resource $bg i3wm.background ${backgroundColor}
          set_from_resource $fg i3wm.foreground ${foregroundColor}
          set_from_resource $c1 i3wm.color1 ${backgroundColor}
          set_from_resource $c2 i3wm.color2 ${foregroundColor}
        '';
        config = {
          assigns = { "9" = [{ class = "^MEGAsync$"; }]; };
          floating.criteria = [{ class = "^MEGAsync$"; }];
          bars = [];
          fonts = [ "Font Awesome" "${font} 11" ];
          gaps = {
            outer = 10;
            inner = 10;
          };
          colors = {
            focused = {
              background = "$c2";
              border = "$c2";
              text = "$fg";
              indicator = "$c2";
              childBorder = "$c2";
            };
            focusedInactive = {
              background = "$c1";
              text = "$fg";
              border = "$c1";
              indicator = "$c1";
              childBorder = "$c1";
            };
            unfocused = {
              background = "$c1";
              border = "$c2";
              text = "$fg";
              indicator = "$c1";
              childBorder = "$c1";
            };
          };
          modifier = "Mod4";
          keybindings =
            lib.mkOptionDefault {
              "Mod4+Return" = "exec termite";
              "Mod4+Shift+c" = "kill";
              "Mod4+space" = "exec rofi -show drun";
              "Mod4+n" = "workspace next";
              "Mod4+e" = "workspace prev";
              "Mod4+Shift+e" = "exec i3-msg exit";
              "Mod4+p" = "focus parent";
              "Mod4+Shift+p" = "focus child";
              "Mod1+h" = "focus left";
              "Mod1+n" = "focus down";
              "Mod1+e" = "focus up";
              "Mod1+i" = "focus right";
              "Mod4+r" = "mode resize";
              "Mod1+Shift+h" = "move left";
              "Mod1+Shift+n" = "move down";
              "Mod1+Shift+e" = "move up";
              "Mod1+Shift+i" = "move right";
              "Mod4+t" = "floating toggle";
              "Mod4+x" = "layout toggle all";
              "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '+10%'";
              "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '10%-'";
              "XF86AudioRaiseVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl up";
              "XF86AudioLowerVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl down";
              "XF86AudioMute" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl mute";
              # Open agenda with Super + A
              "Mod4+a" = "exec emacsclient -c -e '(org-agenda-list)(delete-other-windows)(org-agenda-day-view)'";
              # lock screen with Super + L
              "Mod4+l" = "exec ${lockCmd}";
              # Change wallpaper
              "Mod4+w" = "exec ${pkgs.pywal}/bin/wal -i ${/home/jon/Bildujo/Ekranfonoj} -o ${../scripts/pywal-reload.sh}";
            };
          modes = {
            resize = {
              h = "resize shrink width 10 px or 10 ppt";
              n = "resize grow height 10 px or 10 ppt";
              e = "resize shrink height 10 px or 10 ppt";
              i = "resize grow width 10 px or 10 ppt";
              Escape = "mode default";
            };
          };
          startup = [
            { command = "${pkgs.pywal}/bin/wal -R"; notification = false; }
            { command = "megasync"; notification = false; }
            { command = "xrdb -merge ~/.cache/wal/colors.Xresources"; notification = false; }
            { command = "setxkbmap -layout us -variant colemak -option caps:escape -option esperanto:colemak"; }
            # { command = "exec systemctl --user import-environment"; always = true; notification = false; }
            { command = "exec systemctl --user restart polybar"; always = true; notification = false; }
            # { command = "${pkgs.gnome3.gnome_settings_daemon}/libexec/gsd-xsettings"; }
          ];
          window.border = 10;
        };
    };
  };
}
