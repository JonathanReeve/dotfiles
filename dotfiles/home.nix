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
in
{

  # imports = [  ./minimal.nix ]; 

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
        # notmuch.enable = true;
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
    neomutt = {
      enable = true;
      vimKeys = true;
      extraConfig = ''
      color normal white default
      color attachment red default
      color hdrdefault cyan default
      color indicator brightyellow default
      color markers brightred default
      color quoted cyan default
      color quoted1 magenta default
      color quoted2 blue default
      color signature yellow default
      color status default default
      color tilde blue default
      color tree brightred default
      color header brightyellow default ^From:
      color header yellow default ^To:
      color header brightcyan default ^Date
      color header yellow default ^Cc:
      color header brightgreen default ^Subject:
      color header brightcyan default ^X-TRASH:
      color status brightgreen default
      '';
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
        nnoremap N J
        nnoremap J N
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
         # "pw" = "vim ~/Dokumentujo/Personal/.p10.txt";
         # "lock" = "${lockCmd}";
         "new-session" = "dbus-send --system --type=method_call --print-reply --dest=org.freedesktop.DisplayManager $XDG_SEAT_PATH org.freedesktop.DisplayManager.Seat.SwitchToGreeter";
         "portrait-monitor" = "xrandr --output DP-1 --rotate left --auto --right-of eDP-1";
         # Use vim as pager for manfiles, since it's prettier
       };
       shellAliases = {
         "man" = "env PAGER=\"vim -R -c 'set ft=man'\" man";
         "qutebrowser" = "/usr/bin/qutebrowser";
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
         rename-pdf="mv (ls -t /tmp/*.pdf | head -n 1) ~/Dokumentujo/Papers/$argv.pdf";
         find-book="for engine in b c libgen; qutebrowser \":open -t $engine $argv\"; end";
         # Search several search engines at once. `search b g l "search query"`
         search="for engine in $argv[1..-2]; qutebrowser \":open -t $engine $argv[-1]\"; end";
         # Proverbs for greeting
         fish_greeting = "shuf -n 1 ${scripts}/proverboj.txt | ${pkgs.neo-cowsay}/bin/cowsay";

       };
       interactiveShellInit =
         ''
            # Use Fisher for plugin management
            if not functions -q fisher
                set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
                curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
                fish -c fisher
            end

            # Don't use vi keybindings in unknown terminals,
            # since weird things can happen. Also don't do colors.
            set acceptable_terms xterm-256color screen-256color xterm-termite
            if contains $TERM $acceptable_terms
              fish_vi_key_bindings
              # Load pywal colors
              cat ~/.cache/wal/sequences
            end

            set -U vaultmount ~/.private-mount
            set -U vaultloc ~/Dokumentujo/Personal/.Vault_encfs
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

            eval (direnv hook fish)
         '';
    };
    fzf = {
      enable = true;
      enableFishIntegration = true;
    };
    qutebrowser = {
      enable = true;
      extraConfig = ''
        c.statusbar.padding = {'top': 5, 'bottom': 5, 'left': 3, 'right': 3}
        c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 2, 'right': 2}
      '';
      keyBindings = {
        normal = {
        "N" =  "tab-next";
        "E" =  "tab-prev";
        "K" =  "search-prev";
        "l" =  "enter-mode insert";
        "n" =  "scroll down";
        "e" =  "scroll up";
        "i" =  "scroll right";
        "j" =  "search-next";
        "b" =  "set-cmd-text -s :buffer";
        "gL" =  "spawn --userscript org-link";
        "gM" =  "spawn --userscript org-movie";
        "pf" =  "spawn --userscript qute-pass";
        "gz" =  "jseval var d=document,s=d.createElement('script';;s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement;.appendChild(s;;void(0;;";
        "t" =  "set-cmd-text -s :open -t";
        "Y" =  "yank selection";
        "O" =  "set-cmd-text :open {url:pretty}";
        "<Alt-Left>" =  "back";
        "<Alt-Right>" =  "forward";
        };
      };
      searchEngines = {
        "DEFAULT" =  "https://duckduckgo.com/?q={}";
        "g" =  "https://www.google.com/search?q={}";
        "l" =  "https://www.google.com/search?hl=en&q={}&btnI=I";
        "w" =  "https://en.wikipedia.org/w/index.php?search={}";
        "gs" =  "https://scholar.google.com/scholar?q={}";
        "b" =  "https://www.google.com/search?tbm=bks&q={}";
        "aw" =  "https://wiki.archlinux.org/?search={}";
        "o" =  "https://nixos.org/nixos/options.html#{}";
        "p" =  "https://nixos.org/nixos/packages.html#{}";
        "d" =  "https://en.wiktionary.org/wiki/{}";
        "s" =  "http://stackoverflow.com/search?q={}";
        "m" =  "https://maps.google.com/maps?q={}";
        "c" =  "https://clio.columbia.edu/quicksearch?q={}";
        "gh" =  "https://github.com/search?q={}&type=Repositories";
        "h" =  "https://hackage.haskell.org/packages/search?terms={}";
        "libgen" =  "https://libgen.is/search.php?req={}";
        "viki" =  "https://eo.wikipedia.org/w/index.php?search={}";
        "ia" =  "https://archive.org/details/texts?and%5B%5D={}&sin=";
        "mm" =  "https://muse-jhu-edu.ezproxy.cul.columbia.edu/search?action=search&query=content:{}:and&limit=journal_id:131&min=1&max=10&t=search_journal_header";
        };
      settings = {
        colors = {
          completion.category.bg = "#333333";
          tabs = {
            even.bg = "#222222";
            odd.bg = "#222222";
            selected.even.bg = "#285577";
            selected.odd.bg = "#285577";
          };
        };
        fonts = {
          completion.category = "11pt monospace";
          default_family = "${font}, Terminus, Monospace, monospace, Fixed";
          prompts = "11pt monospace";
        };
        hints.chars = "arstdhneio";
        url.default_page = "${scripts}/homepage/homepage.html";
      };
    };
    # waybar = {
    #   enable = true;
    # };
    # vscode = {
    #   enable = true;
    #   extensions = with pkgs.vscode-extensions; [
    #     ms-python.python
    #     vscodevim.vim
    #   ];
    # haskell = {
    #   enable = true;
    # };
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
      # This should only be necessary with non-NixOS
      keyboard = {
        options = [ "caps:escape" "esperanto:colemak" ];
        variant = "colemak";
      };
      packages = with pkgs; [
        fira-code
        ripgrep
        vale
        pass
        encfs
        fd
        gotop
        tree
        bat
        # mu
        direnv

        (haskellPackages.ghcWithPackages (ps: with ps; [
          pandoc-citeproc
          shake         # Build tool
          hlint         # Required for spacemacs haskell-mode
          apply-refact  # Required for spacemacs haskell-mode
          hasktags      # Required for spacemacs haskell-mode
          hoogle        # Required for spacemacs haskell-mode
          lucid
          # stylish-haskell # Required for spacemacs haskell-mode
          # ^ marked as broken
          turtle
          regex-compat
          # PyF
          HandsomeSoup
        ]))
        cabal-install
      ];
      file = {
        ".doom.d/" = {
          source = ./emacs/doom.d;
          recursive = true;
          onChange = "$HOME/.emacs.d/bin/doom sync";
        };

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
      language = {
        base = "eo";
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
  };
}
