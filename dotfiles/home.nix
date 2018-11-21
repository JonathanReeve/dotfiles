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
    useGtkTheme = true;
  };

  services = {
    gpg-agent = {
      enable = true;
      # Don't ask for password all the time.
      defaultCacheTtl = 31536000; # 365 days
      maxCacheTtl = 31536000;
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
