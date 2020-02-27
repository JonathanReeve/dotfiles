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
  # myNurExpressions = import <nur-jomik> { inherit pkgs; };
  lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -p -t ''";

  # Doom setup following https://github.com/vlaci/nix-doom-emacs
  # doom-emacs = pkgs.callPackage (builtins.fetchTarball {
  #  url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  # }) {
  #  doomPrivateDir = ./emacs/doom.d;  # Directory containing your config.el init.el
  #                                    # and packages.el files
  # };

in
{

  imports = [  ./minimal.nix ];

  # nixpkgs.overlays = [
  #   (self: super: {
  #     nur.repos.jomik = myNurExpressions.pkgs;
  #   })
  # ];

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
          create = "maildir";
          expunge = "both";
          patterns = [ "*" "![Gmail]/*" "\"[Gmail]/Sent Mail\"" "[Gmail]/Lists"];
          extraConfig.channel = {
            MaxMessages = 2000;
            ExpireUnread = "yes";
          };
        };
        realName = "${name}";
        neomutt.enable = true;
      };
      columbia = {
        address = "jonathan.reeve@columbia.edu";
        userName = "jpr2152@columbia.edu";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass lionmail";
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
          patterns = [ "*" "!\"[Gmail]/All Mail\"" "[Gmail]/Sent Mail" ];
          extraConfig.channel = {
            MaxMessages = 2000;
            ExpireUnread = "yes";
          };
        };
        realName = "${name}";
        neomutt.enable = true;
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
    # notmuch = {
    #   enable = true;
    # };
    neomutt = {
      enable = true;
      vimKeys = true;
    };
    neovim = {
      enable = true;
      # plugins = [ pkgs.vimPlugins.vim-airline ];
      vimAlias = true;
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
         "edit-home" = "$EDITOR ${dots}/home.nix";
         "edit-conf" = "$EDITOR ${dots}/configuration.nix";
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
         "portrait-monitor" = "xrandr --output DP-1 --rotate left --auto --right-of eDP-1";
         # Use vim as pager for manfiles, since it's prettier
       };
       shellAliases = {
         "man" = "env PAGER=\"vim -R -c 'set ft=man'\" man";
         };
       functions = {
         vault="encfs $vaultloc $vaultmount";
         unvault="fusermount -u $vaultmount";
         jnl="vault; and emacsclient -c $vaultmount/Journal/jnl.org; and unvault";
         upgrade=''
            sudo -i nixos-rebuild switch --upgrade; and nix-env -u;
            and home-manager switch --upgrade;
            '';
         clean = "nix-store --gc --print-roots; and sudo nix-collect-garbage --delete-older-than 5d";
         # A function for renaming the most recent PDF, and putting it in my Papers dir.
         rename-pdf="mv (ls -t /tmp/*.pdf | head -n 1) ~/Dropbox/Papers/$argv.pdf";
         find-book="for engine in b c libgen; qutebrowser \":open -t $engine $argv\"; end";
         # Search several search engines at once. `search b g l "search query"`
         search="for engine in $argv[1..-2]; qutebrowser \":open -t $engine $argv[-1]\"; end";
         # Proverbs for greeting
         fish_greeting = "shuf -n 1 ${scripts}/proverboj.txt | ${pkgs.cowsay}/bin/cowsay";

       };
       interactiveShellInit =
         ''
            # Don't use vi keybindings in unknown terminals,
            # since weird things can happen. Also don't do colors.
            set acceptable_terms xterm-256color screen-256color xterm-termite
            if contains $TERM $acceptable_terms
              fish_vi_key_bindings
              # Load pywal colors
              cat ~/.cache/wal/sequences
            end

            set -U vaultmount ~/.private-mount
            set -U vaultloc ~/Dropbox/Personal/.Vault_encfs
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
    enable = true;
    # useGtkTheme = true;
  };

  # Dotfiles for the home root, ~/
  home = {
      keyboard = {
        options = [ "caps:escape" "esperanto:colemak" ];
        variant = "colemak";
      };
      packages = with pkgs; [
        # Command-line enhancements
        bat fd fzf
        # Spacemacs email
        w3m mu
        # Encryption
        encfs
        # File sync and backup
        # megasync megatools
        # doom-emacs
      ];
      file = {
        # Handle multiple emacs installs
        ".emacs-profiles.el".source = ./emacs/emacs-profiles.el;
        ".spacemacs".source = ./emacs/spacemacs;
        ".doom.d/" = {
          source = ./emacs/doom.d;
          recursive = true;
        };
        #".emacs.d/init.el".text = ''
        #  (load "default.el")
        #'';

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
      "qutebrowser/config.py".text =
      ''
        c.colors.completion.category.bg = "#333333"
        c.colors.tabs.even.bg = '#222222'
        c.colors.tabs.odd.bg = '#222222'
        c.colors.tabs.selected.even.bg = '#285577'
        c.colors.tabs.selected.odd.bg = '#285577'
        c.fonts.completion.category = '11pt monospace'
        c.fonts.default_family = '${font}, Terminus, Monospace, monospace, Fixed'
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
                'libgen': 'https://libgen.is/search.php?req={}',
                'viki': 'https://eo.wikipedia.org/w/index.php?search={}',
                'ia': 'https://archive.org/details/texts?and%5B%5D={}&sin=',
                'mm': 'https://muse-jhu-edu.ezproxy.cul.columbia.edu/search?action=search&query=content:{}:and&limit=journal_id:131&min=1&max=10&t=search_journal_header'
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
        config.bind('b', 'set-cmd-text -s :buffer')
        config.bind('gL', 'spawn --userscript org-link')
        config.bind('pf', 'spawn --userscript qute-lastpass')
        config.bind('gz', "jseval var d=document,s=d.createElement('script');s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement).appendChild(s);void(0);")
        config.bind('t', 'set-cmd-text -s :open -t')
        config.bind('Y', 'yank selection')
        config.bind('O', 'set-cmd-text :open {url:pretty}')
        config.bind('<Alt-Left>', 'back')
        config.bind('<Alt-Right>', 'forward')
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
        # config.source('qutewal.py')
      '';
    };
  };
}
