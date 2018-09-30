{pkgs, ...}:

let
  # Personal Info
  name = "Jonathan Reeve";
  email = "jon.reeve@gmail.com";
  githubUsername = "JonathanReeve";
  # Paths
  dots = "/home/jon/Dotfiles/dotfiles";
  scripts = "/home/jon/Dotfiles/scripts";
  maildir = "/home/jon/Mail";
  # Preferences
  font = "Hack";
  backgroundColor = "#243442"; # Blue steel
  foregroundColor = "#deedf9"; # Light blue
  warningColor = "#e23131"; # Reddish
  lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -p -t ''";
in
{
  accounts.email = {
    maildirBasePath = "${maildir}";
    accounts = {
      gmail = {
        address = "${email}";
        userName = "${email}";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass gmail";
        primary = true;
        mbsync = {
          enable = true;
          expunge = "both";
          patterns = [ "*" "![Gmail]*" "[Gmail]/Sent Mail" ];
        };
        realName = "${name}";
      };
      columbia = {
        address = "jonathan.reeve@columbia.edu";
        userName = "jpr2152@columbia.edu";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass lionmail";
        mbsync = {
          enable = true;
          expunge = "both";
          patterns = [ "*" "![Gmail]*" "[Gmail]/Sent Mail" ];
        };
        realName = "${name}";
      };
    };
  };
  programs = {
    # Have home-manager manage itself.
    home-manager = {
      enable = true;
      path = "/home/jon/Code/home-manager";
    };
    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
    };
    mbsync = {
      enable = true;
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
         "glg" = "git log --color --graph --pretty --oneline";
         "glgb" = "git log --all --graph --decorate --oneline --simplify-by-decoration";
         "gst" = "git status";
         # Other abbreviations
         "em" = "emacsclient -c";
         "pw" = "vim ~/Dropbox/Personal/.p10.txt";
         "lock" = "${lockCmd}";
         "new-session" = "dbus-send --system --type=method_call --print-reply --dest=org.freedesktop.DisplayManager $XDG_SEAT_PATH org.freedesktop.DisplayManager.Seat.SwitchToGreeter";
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

            function upgrade
              sudo -i nixos-rebuild switch --upgrade
              and nix-env -u
              and home-manager switch --upgrade
            end
            funcsave upgrade

            function clean
              nix-store --gc --print-roots
              and sudo nix-collect-garbage --delete-older-than 5d
            end
            funcsave clean
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
      backgroundColor = "rgba(32, 39, 51, 0.3)";
    };
    zathura = {
      enable = false;
      options = {
        font = "${font} 11";
        page-padding = 0;
        selection-clipboard = "clipboard";
        statusbar-h-padding = 20;
      };
    };
  };

  xsession = {
    enable = true;
    pointerCursor = {
        package = pkgs.vanilla-dmz;
        name = "Vanilla-DMZ";
        size = 48;
    };
    # windowManager.command = "${pkgs.bspwm}/bin/bspwm";
    windowManager.bspwm = {
      enable = true;
      config = {
        # "normal_border_color" = "$color1";
        # "active_border_color" = "$color2";
        # "focused_border_color" = "$color15";
        # "presel_feedback_color" = "$color1";
        "border_width" = 15;
        "window_gap" = 25;
        "split_ratio" = 0.52;
        "gapless_monocle" = true;
        "focus_follows_pointer" = true;
      };
      extraConfig = ''
        bspc monitor -d 1 2 3 4 5 6 7 8 9 10
        bspc rule -a emacs desktop='^1'
        bspc rule -a qutebrowser desktop='^2'
        bspc rule -a Gimp desktop='^8' state=floating follow=on
      '';
      monitors = [{
        name = "eDP0";
        desktops = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ];
      }];
      startupPrograms = [
        "xrdb -merge ~/.extend.Xresources"
        "xsetroot -cursor_name left_ptr"
        "sxhkd"
        "wal -R -o ${scripts}/pyal-reload-everything.sh"
        "${scripts}/notifications.sh"
        ];
    };
    scriptPath = ".xsession-hm";
    profileExtra =
    ''
      xrdb -merge ~/.extend.Xresources
      bass source ~/.nix-profile/etc/profile.d/hm-session-vars.sh
    '';
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
      package = pkgs.breeze-gtk;
      name = "Breeze-Dark";
    };
    iconTheme = {
      package = pkgs.breeze-icons;
      name = "breeze-dark";
    };
    # Give Termite some internal spacing.
    gtk3.extraCss = ".termite {padding: 20px;}";
  };

  qt = {
    enable = false;
    useGtkTheme = true;
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
    compton = {
      enable = true;
      blur = true;
      shadow = true;
    };
    polybar = {
      enable = true;
      script = "polybar main_bar &";
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
          click-left = "${pkgs.emacs}/bin/emacsclient --eval '(org-clock-out)' && echo ' Stopped!'";
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
    screen-locker = {
      enable = true;
      lockCmd = "${lockCmd}";
    };
    mpd = {
      enable = true;
      musicDirectory = ~/Music;
    };
  };

  # Adapted from https://github.com/yrashk/nix-home/blob/master/home.nix#L194
  systemd.user = {
    services = {
      dropbox = {
        Unit = {
          Description = "Dropbox";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Restart = "on-failure";
          RestartSec = 1;
          ExecStart = "${pkgs.dropbox}/bin/dropbox";
          Environment = "QT_PLUGIN_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}";
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
      # syncmail = {
      #   Unit = {
      #     Description = "Sync email and index with mu";
      #   };
      #   Service = {
      #     Type = "oneshot";
      #     ExecStart = "${pkgs.isync}/bin/mbsync -a";
      #     ExecStartPost = "${pkgs.mu}/bin/mu index -m ${maildir}";
      #     SuccessExitStatus = "0 1";
      #   };
      # };
      # cleanmail = {
      #   Unit = {
      #     Description = "Sync email and index with mu";
      #   };
      #   Service = {
      #     Type = "oneshot";
      #     ExecStart = "${pkgs.isync}/bin/mbsync -dXa";
      #     ExecStartPost = "${pkgs.mu}/bin/mu index -m ${maildir}";
      #     SuccessExitStatus = "0 1";
      #   };
      # };
    };
    # timers = {
    #   syncmail = {
    #     Unit = {
    #       Description = "Schedule syncing email and indexing with mu";
    #     };
    #     Timer = {
    #       Unit = "syncmail.service";
    #       OnCalendar = "*:0/15";
    #     };
    #     Install = {
    #       WantedBy = [ "timers.target" ];
    #     };
    #   };
    #   cleanmail = {
    #     Unit = {
    #       Description = "Schedule expunging email and indexing with mu";
    #     };
    #     Timer = {
    #       Unit = "cleanmail.service";
    #       OnCalendar = "daily";
    #     };
    #     Install = {
    #       WantedBy = [ "timers.target" ];
    #     };
    #   };
    # };
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
        ".stack/config.yaml".text =
        ''
          templates:
            params:
              author-name: ${name}
              author-email: ${email}
              copyright: ${name}
              github-username: ${githubUsername}
          nix:
            enable: true
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
      # BSPWM stuff
      "sxhkd/sxhkdrc".source = ./sxhkdrc;
      # "bspwm/bspwmrc".source = ./bspwmrc;
      "qutebrowser/config.py".text =
      ''
        c.colors.completion.category.bg = "#333333"
        c.colors.tabs.even.bg = '#222222'
        c.colors.tabs.odd.bg = '#222222'
        c.colors.tabs.selected.even.bg = '#285577'
        c.colors.tabs.selected.odd.bg = '#285577'
        c.fonts.completion.category = '10pt monospace'
        c.fonts.monospace = '${font}, Terminus, Monospace, monospace, Fixed'
        c.fonts.prompts = '10pt monospace'
        c.hints.chars = 'arstdhneio'
        c.statusbar.padding = {'top': 10, 'bottom': 10, 'left': 5, 'right': 5}
        c.tabs.padding = {'top': 4, 'bottom': 4, 'left': 4, 'right': 4}
        c.url.searchengines = {
                'DEFAULT': 'https://duckduckgo.com/?q={}',
                'g': 'https://www.google.com/search?q={}',
                'l': 'https://www.google.com/search?hl=en&q={}&btnI=I',
                'w': 'https://en.wikipedia.org/w/index.php?search={}',
                'gs': 'https://scholar.google.com/scholar?q={}',
                'b': 'https://www.google.com/search?tbm=bks&q={}',
                'aw': 'https://wiki.archlinux.org/?search={}',
                'o': 'https://nixos.org/nixos/options.html#{}',
                'p': 'https://nixos.org/nixos/packages.html#{}',
                'd': 'https://en.wiktionary.org/wiki/{}',
                's': 'http://stackoverflow.com/search?q={}',
                'm': 'https://maps.google.com/maps?q={}',
                'c': 'https://clio.columbia.edu/quicksearch?q={}',
                'gh': 'https://github.com/search?q={}&type=Repositories',
                'h': 'https://hackage.haskell.org/packages/search?terms={}'
                }
        config.bind('N', 'tab-next')
        config.bind('E', 'tab-prev')
        config.bind('K', 'search-prev')
        config.bind('l', 'enter-mode insert')
        config.bind('n', 'scroll down')
        config.bind('e', 'scroll up')
        config.bind('i', 'scroll right')
        config.bind('j', 'search-next')
        config.bind('gL', 'spawn --userscript org-link')
        config.bind('pf', 'spawn --userscript password_fill')
        config.bind('t', 'set-cmd-text -s :open -t')
        config.bind('Y', 'yank selection')
        config.bind('O', 'set-cmd-text :open {url:pretty}')
        # Hack for repeating a search, but with a different search engine.
        c.aliases['repeat-search'] = ';;'.join([
            'set-cmd-text :',        # enter command mode
            'command-history-prev',  # this command
            'command-history-prev',  # search command
            'rl-beginning-of-line',  # :|open engine term1 term2
            'rl-forward-word',       # :open |engine term1 term2
            'rl-forward-word',       # :open engine |term1 term2
            'rl-backward-char',      # :open engine| term1 term2
            'rl-backward-kill-word'  # :open | term1 term2
        ])
      '';
    };
  };
}
