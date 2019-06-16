{pkgs, lib, ...}:

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
  font = "Fira Code";
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
      path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    };
    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
    };
    mbsync = {
      enable = true;
    };
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
    vim = {
      enable = true;
      plugins = [ "vim-airline" ];
      settings = { ignorecase = true; };
      extraConfig =
      ''
        set mouse=a
        " Colemak some things
        nnoremap n j
        nnoremap j n
        nnoremap e k
        nnoremap k e
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
            end

            if not functions -q fisher
                set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
                curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
                fish -c fisher
            end

            set -U vaultmount ~/.private-mount
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

            # A function for renaming the most recent PDF, and putting it in my Papers dir.
            function rename-pdf
              mv (ls -t /tmp/*.pdf | head -n 1) ~/Dropbox/Papers/$argv.pdf
            end
            funcsave rename-pdf

            function find-book
              for engine in b c libgen
                qutebrowser ":open -t $engine $argv"
              end
            end
            funcsave find-book

         '';
       promptInit =
         ''
            # Disable the vim-mode indicator [I] and [N].
            # Let the theme handle it instead.
            function fish_default_mode_prompt; true; end

            # This doesn't seem to work below for some reason.
            function fish_title; true; end

            # Emacs ansi-term support
            if test -n "$EMACS"
              set -x TERM eterm-color
              # Disable right prompt in emacs
              # function fish_right_prompt; true; end
              function fish_title; true; end
            end
         '';
    };
  };


  gtk = {
    enable = false;
    theme = {
      package = pkgs.breeze-gtk;
      name = "Breeze-Dark";
    };
    iconTheme = {
      package = pkgs.breeze-icons;
      name = "breeze-dark";
    };
    # Give Termite some internal spacing.
    gtk3.extraCss = ".termite {padding: 10px;}";
  };

  qt = {
    enable = false;
    # useGtkTheme = true;
  };

  # Adapted from https://github.com/yrashk/nix-home/blob/master/home.nix#L194
  systemd.user = {
    services = {
      # gpg-agent = {
      #   enable = true;
        # Don't ask for password all the time.
        # defaultCacheTtl = 31536000; # 365 days
        # maxCacheTtl = 31536000;
      # };
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
      keyboard = {
        options = [ "caps:escape" ];
        variant = "colemak";
      };
      file = {
        ".spacemacs".source = ./spacemacs;

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
      package = pkgs.polybar.override {
        i3GapsSupport = true;
        alsaSupport = true;
        githubSupport = true;
      };
      script = "polybar main_bar &";
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
           font-2 = "NotoEmoji:size=11;1";
           modules-left = "i3 xwindow";
           modules-center = "date";
           modules-right = "org-clock volume backlight filesystem memory cpu battery network";
        };
        "module/i3" = {
          type = "internal/i3";
          label-focused-underline = "\${xrdb:color4}";
          label-focused = "%index%";
          label-unfocused = "%index%";
          label-focused-padding = 1;
          label-unfocused-padding = 1;
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
          type = "internal/xbacklight";
          format = "<ramp>";
          #ramp-0 = "🌕";
          #ramp-1 = "🌔";
          #ramp-2 = "🌓";
          #ramp-3 = "🌒";
          #ramp-4 = "🌑";
        };
      };
    };
    screen-locker = {
      enable = true;
      lockCmd = "${lockCmd}";
    };
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
      "qutebrowser/config.py".text =
      ''
        c.colors.completion.category.bg = "#333333"
        c.colors.tabs.even.bg = '#222222'
        c.colors.tabs.odd.bg = '#222222'
        c.colors.tabs.selected.even.bg = '#285577'
        c.colors.tabs.selected.odd.bg = '#285577'
        c.fonts.completion.category = '11pt monospace'
        c.fonts.monospace = '${font}, Terminus, Monospace, monospace, Fixed'
        c.fonts.prompts = '11pt monospace'
        c.hints.chars = 'arstdhneio'
        c.statusbar.padding = {'top': 5, 'bottom': 5, 'left': 3, 'right': 3}
        c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 2, 'right': 2}
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
                'h': 'https://hackage.haskell.org/packages/search?terms={}',
                'libgen': 'https://ambry.pw/search?q={}',
                'viki': 'https://eo.wikipedia.org/w/index.php?search={}',
                'viki': 'https://eo.wikipedia.org/w/index.php?search={}',
                }
        c.url.default_page = "${scripts}/homepage/homepage.html";
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
        config.bind('gz', "jseval var d=document,s=d.createElement('script');s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement).appendChild(s);void(0);")
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
  xsession = {
    enable = true;
    scriptPath = ".xsession-hm";
    # windowManager.command = "bspwm";
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
    };
    windowManager.i3 = {
      enable = true;
      extraConfig = ''
        set_from_resource $bg i3wm.background
        set_from_resource $fg i3wm.foreground
        set_from_resource $c1 i3wm.color1
        set_from_resource $c2 i3wm.color2
      '';
      config = {
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
            "Mod1+h" = "focus left";
            "Mod1+n" = "focus down";
            "Mod1+e" = "focus up";
            "Mod1+i" = "focus right";
            "Mod1+Shift+h" = "move left";
            "Mod1+Shift+n" = "move down";
            "Mod1+Shift+e" = "move up";
            "Mod1+Shift+i" = "move right";
            "Mod4+t" = "floating toggle";
            "Mod4+x" = "layout toggle all";
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
          { command = "systemctl --user restart polybar"; always = true; notification = false; }
          # { command = "dropbox start"; notification = false; }
          { command = "wal -R"; notification = false; }
          { command = "xrdb -merge ~/.cache/wal/colors.Xresources"; notification = false; }
        ];
        window.border = 10;
      };
    };
  };
}
