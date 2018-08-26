{pkgs, ...}:

let
  dots = "/home/jon/Dotfiles/dotfiles";
  scripts = "/home/jon/Dotfiles/scripts";
  font = "Hack";
in
{
  programs = {
    # Have home-manager manage itself.
    home-manager = {
      enable = true;
      path = "/home/jon/Code/home-manager";
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
     fish = {
       enable = true;
       shellAbbrs = {
         # Git abbreviations
         "ga" = "git add";
         "gc" = "git commit";
         "gcam" = "git commit -am";
         "gcm" = "git commit -m";
         "gco" = "git checkout";
         "gcob" = "git checkout -b";
         "gcom" = "git checkout master";
         "gcod" = "git checkout develop";
         "gd" = "git diff";
         "gp" = "git push";
         "gdc" = "git diff --cached";
         "glg" = "git lg";
         "gst" = "git status";
         # Other abbreviations
         "em" = "emacsclient -c";
         "pw" = "vim ~/Dropbox/Personal/.p10.txt";
       };

       interactiveShellInit =
         ''
            # Don't use vi keybindings in unknown terminals,
            # since weird things can happen.
            set acceptable_terms xterm-256color screen-256color xterm-termite
            if contains $TERM $acceptable_terms
              fish_vi_key_bindings
              # Load pywal colors
              cat ~/.cache/wal/sequences
            end

            set -U vaultmount ~/Documents/Settings/.private-mount
            set -U vaultloc ~/Dropbox/Personal/.Vault_encfs 

            alias vault="encfs $vaultloc $vaultmount"
            alias unvault="fusermount -u $vaultmount"
            funcsave vault
            funcsave unvault

            function jnl
              vault
              and emacsclient -c $vaultmount/Journal/jnl.org
              and unvault
            end

            funcsave jnl
         '';
       promptInit =
         ''
            # Emacs ansi-term support
            if test -n "$EMACS"
                set -x TERM eterm-color
                # Disable right prompt in emacs
                function fish_right_prompt; true; end
                #This function may be required for Emacs support.
                function fish_title; true; end
            end

            # Disable the vim-mode indicator [I] and [N].
            # Let the theme handle it instead.
            function fish_default_mode_prompt; true; end

            # chips
            if [ -e ~/.config/chips/build.fish ] ; . ~/.config/chips/build.fish ; end
         '';
    };
    rofi = {
      enable = true;
      font = "${font} 20";
      theme = "~/.cache/wal/colors-rofi-dark.rasi";
    };
    termite = {
      enable = true;
      font = "${font} 11";
    };
  };

  # xsession = {
  #   enable = true;
  #   pointerCursor = {
  #       package = pkgs.vanilla-dmz;
  #       name = "Vanilla-DMZ";
  #       size = 64;
  #   };
  #   windowManager.command = "bspwm";
  #   profileExtra =
  #   ''
  #     xrdb -merge ~/.extend.Xresources
  #   '';
  # };

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
    # Give Termite some internal spacing. 
    gtk3.extraCss = ".termite {padding: 20px;}";
  };

  qt = {
    enable = true;
    useGtkTheme = true;
  };

  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          monitor = 0;
          follow = "mouse";
          geometry = "250x50-30+58";
          indicate_hidden = "yes";
          shrink = "no";
          separator_height = 0;
          padding = 16;
          horizontal_padding = 24;
          frame_width = 2;
          sort = "no";
          idle_threshold = 120;
          font = "${font} 10";
          line_height = 4;
          markup = "full";
          format = "<b>%s</b>\n%b";
          alignment = "left";
          show_age_threshold = 60;
          word_wrap = "yes";
          ignore_newline = "no";
          stack_duplicates = "false";
          hide_duplicate_count = "yes";
          show_indicators = "no";
          icon_position = "off";
          sticky_history = "yes";
          history_length = "20";
          always_run_script = "true";
          };
        shortcuts = {
          close = "ctrl+space";
          close_all = "ctrl+shift+space";
          history = "ctrl+grave";
          context = "ctrl+shift+period";
        };
        urgency_low = {
          timeout = 4;
        };
        urgency_normal = {
          timeout = 8;
        };
        urgency_critical = {
          timeout = 0;
        };
      };
    };
    compton = {
      # Disable this for now
      enable = false;
      blur = true;
    };
    polybar = {
      enable = false;
      script = "xrdb -merge ~/.extend.Xresources && polybar main_bar";
      config = {
        "bar/main_bar" = {
           monitor = "eDP1";
           bottom = "false";
           height = 50;
           fixed-center = "true";
           background = "\${xrdb:background}";
           foreground = "\${xrdb:foreground}";
           line-size = 6;
           line-color = "\${xrdb:color4}";
           padding-right = "1%";
           module-margin-left = 1;
           module-margin-right = 1;
           font-2 = "FontAwesome:fontformat=truetype:size=19;1";
           font-1 = "NotoEmoji:fontformat=truetype:size=19;1";
           font-0 = "${font}:fontformat=truetype:size=19;1";
           wm-restack = "bspwm";
           modules-left = "bspwm xwindow";
           modules-center = "date";
           modules-right = "org-clock volume backlight filesystem memory cpu battery network";
        };
        "module/bspwm" = {
          type = "internal/bspwm";
          format = "<label-state> <label-mode>";
          label-monocle = "M";
          label-floating = "S";
          fuzzy-match = "true";
          ws-icon-0 = "1;";
          ws-icon-1 = "2;";
          ws-icon-2 = "3;";
          ws-icon-3 = "4;";
          ws-icon-4 = "5;♞";
          ws-icon-default = "";
          label-mode-padding = "2";
          label-mode-background = "\${xrdb:color4}";
          label-focused = "%icon%";
          label-focused-underline = "\${xrdb:color4}";
          label-focused-padding = 2;
          label-empty = '''';
          label-occupied = "%icon%";
          label-occupied-padding = 2;
          label-urgent = "%icon%";
          label-urgent-underline = "\${xrdb:color8}";
          label-urgent-padding = 2;
        };
        "module/date" = {
          type = "internal/date";
          interval = 5;
          date = "%m-%d %a";
          time = "%H:%M";
          format-prefix-foreground = "\${xrdb:foreground}";
          label = "%date% %time%";
        };
        "module/battery" = {
           type = "internal/battery";
           battery = "BAT1";
           adapter = "ADP1";
           full-at = 96;
           format-charging = " <label-charging>";
           format-discharging = "<ramp-capacity> <label-discharging>";
           format-full = " ";
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
          interface = "wlp1s0";
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
          interval = 10;
          exec = "${scripts}/org-clock.sh";
          click-left = "emacsclient --eval '(org-clock-out)' && echo ' Stopped!'";
        };
        "module/memory" = {
          type = "internal/memory";
          label = " %percentage_used%%";
        };
        "module/filesystem" = {
          type = "internal/fs";
          mount-0 = "/";
          mount-1 = "/home";
          label-mounted = " %percentage_used%%";
        };
        "module/volume" = {
          type = "internal/alsa";
          label-volume = " %percentage%";
          label-muted = "";
          click-left = "pactl set-sink-mute 0 toggle";
        };
        "module/backlight" = {
          type = "internal/backlight";
          format = "<ramp>";
          card = "intel_backlight";
          ramp-0 = "🌕";
          ramp-1 = "🌔";
          ramp-2 = "🌓";
          ramp-3 = "🌒";
          ramp-4 = "🌑";
        };
      };
    };
  };

  # Dotfiles for the home root, ~/
  home = {
      file = {
        ".spacemacs".source = "${dots}/spacemacs";

        # Vim all the things!
        ".inputrc".text =
        ''
          set editing-mode vi
          set keymap vi-command
        '';
      };
      # packages = [
      #     pkgs.glibcLocales
      # ];
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
      # "fish/config.fish".source = "${dots}/config.fish";
      "qutebrowser/config.py".source = "${dots}/qutebrowser.py";

      # BSPWM stuff
      "sxhkd/sxhkdrc".source = "${dots}/sxhkdrc";
      "polybar/config".source = "${dots}/polybar";
      "bspwm/bspwmrc".source = "${dots}/bspwmrc";
    };
  };
}
