{pkgs, lib, ...}:

let
  # Personal Info
  name = "Jonathan Reeve";
  email = "jon.reeve@gmail.com";
  githubUsername = "JonathanReeve";
  # Paths
  dots = "/home/jon/Agordoj/dotfiles";
  dokumentoj = "/home/jon/Dokumentoj";
  scripts = "/home/jon/Agordoj/scripts";
  maildir = "/home/jon/Retpoŝto";
  # Preferences
  font = "Victor Mono";
  # font = "Nova Mono";
  # font = "Fantasque Sans Mono";
  backgroundColor = "#243442"; # Blue steel
  foregroundColor = "#deedf9"; # Light blue
  warningColor = "#e23131"; # Reddish
  lockCmd = "${pkgs.swaylock-fancy}/bin/swaylock-fancy -p -t ''";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
in
{
  imports = [
    # ./nix-doom-emacs.nix
  ];
  accounts.email = {
    maildirBasePath = "${maildir}";
    accounts = {
      gmail = {
        address = "${email}";
        userName = "${email}";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass gmail";
        primary = true;
        mu.enable = true;
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
          patterns = [ "INBOX" "Lists" "[Gmail]/Sent Mail"];
          extraConfig.channel = {
            MaxMessages = 2000;
            ExpireUnread = "yes";
          };
        };
        realName = "${name}";
      };
      columbia = {
        address = "jonathan.reeve@columbia.edu";
        userName = "jpr2152@columbia.edu";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass lionmail";
        mu.enable = true;
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
          patterns = [ "INBOX" "Homework" "Lists" "[Gmail]/Sent Mail"];
          extraConfig.channel = {
            MaxMessages = 2000;
            ExpireUnread = "yes";
          };
        };
        realName = "${name}";
      };
      protonmail = {
        address = "jonathan@jonreeve.com";
        userName = "jonathan@jonreeve.com";
        passwordCommand = "${pkgs.pass}/bin/pass show 127.0.0.1:1143/jonathan@jonreeve.com";
        imap = {
          host = "127.0.0.1";
          port = 1143;
          tls = {
            enable = true;
            useStartTls = true;
            certificatesFile = "/home/jon/.config/protonmail/bridge/cert.pem";
          };
        };
        smtp = {
          host = "127.0.0.1";
          port = 1025;
          tls = {
            enable = true;
            useStartTls = true;
            certificatesFile = "/home/jon/.config/protonmail/bridge/cert.pem";
          };
        };
        mu.enable = true;
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
          patterns = [ "INBOX" "Labels/Lists" "Sent" "Drafts" "Spam" ];
          extraConfig.channel = {
            MaxMessages = 2000;
            ExpireUnread = "yes";
          };
        };
        realName = "${name}";
      };
    };
  };
  programs = {
    alacritty = {
      enable = true;
      settings = {
        font.normal.family = "${font}";
        font.size = 14;
        window.opacity = 0.9;
        colors.transparent_background_colors = true;
        shell = "nu";
      };
    };
    bottom.enable = true;
    broot = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        modal = true;
      };
    };
    emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.mu4e
      ];
    };
    gnome-terminal = {
      profile.default = {
        allowBold = false;
        allowBell = false;
        font = "${font}";
        transparencyPercent = 90;
        themeVariant = "dark";
      };
    };
    # Have home-manager manage itself.
    home-manager = {
      enable = true;
      # path = "/home/jon/Code/home-manager";
    };
    direnv = {
      nix-direnv.enable = true;
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = true;
    };
    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
      extraConfig = {
        pull.rebase = false;
        # url = { "git@github.com:" = { insteadOf = "https://github.com"; }; };
      };
    };
    mbsync = {
      enable = true;
    };
    mu = {
      enable = true;
    };
    neovim = {
      enable = true;
      # package = pkgs.neovim;
      plugins = with pkgs.vimPlugins; [ spacevim ];
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
    niri = {
      enable = true;
      package = pkgs.niri;
    };
    nix-index = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
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
        # "lock" = "${lockCmd}";
        "new-session" = "dbus-send --system --type=method_call --print-reply --dest=org.freedesktop.DisplayManager $XDG_SEAT_PATH org.freedesktop.DisplayManager.Seat.SwitchToGreeter";
        "portrait-monitor" = "xrandr --output DP-1 --rotate left --auto --right-of eDP-1";
        "monitor" = "xrandr --output DP-1 --auto --above eDP-1";
        "monitor-off" = "xrandr --output DP-1 --off";
      };
      shellAliases = {
        # Use vim as pager for manfiles, since it's prettier
        # "man" = "env PAGER=\"vim -R -c 'set ft=man'\" man";
      };
      functions = {
        vault="encfs $vaultloc $vaultmount";
        unvault="fusermount -u $vaultmount";
        jnl="vault; and emacsclient -c $vaultmount/Journal/jnl.org; and unvault";
        upgrade=''
            nix flake update ${dots}
            sudo nixos-rebuild switch --flake ${dots}
            '';
        clean = "nix-store --gc --print-roots; and sudo nix-collect-garbage --delete-older-than 5d";
        # A function for renaming the most recent PDF, and putting it in my Papers dir.
        rename-pdf="mv (ls -t /tmp/*.pdf | head -n 1) ${dokumentoj}/Papers/$argv.pdf";
        find-book="for engine in b c libgen ia; qutebrowser \":open -t $engine $argv\"; end";
        # Search several search engines at once. `search b g l "search query"`
        search="for engine in $argv[1..-2]; qutebrowser \":open -t $engine $argv[-1]\"; end";
        # Proverbs for greeting
        fish_greeting = "shuf -n 1 ${scripts}/proverboj.txt | ${pkgs.neo-cowsay}/bin/cowsay";
        em = "emacsclient -c $argv &; disown";
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
            set -U vaultloc ${dokumentoj}/Personal/.Vault_encfs

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

            #eval (direnv hook fish)
         '';
      # plugins = [
      #   {
      #     name = "z";
      #     src = pkgs.fetchFromGitHub {
      #       owner = "jethrokuan";
      #       repo = "z";
      #       rev = "45a9ff6d0932b0e9835cbeb60b9794ba706eef10";
      #       sha256 = "pWkEhjbcxXduyKz1mAFo90IuQdX7R8bLCQgb0R+hXs4=";
      #     };
      #   }
      #   {
      #     name = "bang-bang";
      #     src = pkgs.fetchFromGitHub {
      #       owner = "oh-my-fish";
      #       repo = "plugin-bang-bang";
      #       rev = "f969c618301163273d0a03d002614d9a81952c1e";
      #       sha256 = "A8ydBX4LORk+nutjHurqNNWFmW6LIiBPQcxS3x4nbeQ=";
      #     };
      #   }
      #   {
      #     name = "fzf.fish";
      #     src = pkgs.fetchFromGitHub {
      #       owner = "PatrickF1";
      #       repo = "fzf.fish";
      #       rev = "0dc2795255d6ac0759e6f1d572f64ea0768acefb";
      #       sha256 = "fl4/Pgtkojk5AE52wpGDnuLajQxHoVqyphE90IIPYFU=";
      #     };
      #   }
      #   {
      #     name = "bass";
      #     src = pkgs.fetchFromGitHub {
      #       owner = "edc";
      #       repo = "bass";
      #       rev = "2fd3d2157d5271ca3575b13daec975ca4c10577a";
      #       sha256 = "fl4/Pgtkojk5AE52wpGDnuLajQxHoVqyphE90IIPYFU=";
      #     };
      #   }
      # ];
    };
    fzf = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
    };
    nushell = {
      enable = true;
      configFile.source = ./config.nu;
      environmentVariables = {
        PASSWORD_STORE_DIR = "${dokumentoj}/Personal/.password-store";
      };
      shellAliases = {
        # upgrade = "nix flake update ${dots}; sudo nixos-rebuild switch --flake ${dots}";
      };
    };
    starship = {
      enable = true;
      enableFishIntegration = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
    zsh = {
      enable = true;
      enableCompletion = true;
      enableVteIntegration = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
    };
    zoxide = {
      enable = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
    termite = {
      enable = true;
      clickableUrl = true;
      # backgroundColor = "\${xrdb:background}";
      backgroundColor = "rgba(32, 45, 56, 0.8)";
      foregroundColor = "\${xrdb:foreground}";
      font = "${font} 14";
    };
    rofi = {
      enable = true;
      theme = "~/.cache/wal/colors-rofi-dark.rasi";
      font = "${font} 12";
      package = pkgs.rofi-wayland;
    };
    waybar = {
      enable = true;
      settings = [{
        layer = "top";
        height = 30;
        modules-left = [ "sway/workspaces" "sway/mode" "sway/window" ];
        modules-center = [ "custom/clock" ];
        modules-right = [ "custom/org-clock" "pulseaudio" "cpu" "memory" "network" "battery" "clock" ];
        battery = {
          format = "{capacity}% {icon}";
          format-icons = [ "" "" "" "" ""];
        };
        cpu = {
          format = "{usage}% ";
          tooltip = false;
        };
        memory = {
          format = "{}% ";
        };
        network = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ipaddr}/{cidr} ";
          tooltip-format = "{ifname} via {gwaddr} ";
          format-linked = "{ifname} (No IP) ";
          format-disconnected = "Disconnected ⚠";
          format-alt = "{ifname}: {ipaddr}/{cidr}";
        };
        "custom/clock" = {
          interval = 10;
          exec = "date '+%a %d %b W%V-%u %R'";
          format-alt = "{:%a, %d. %b  %H:%M}";
        };
        "custom/org-clock" = {
          format = "{}";
          max-length = 80;
          interval = 30;
          exec = "/run/current-system/sw/bin/env ${scripts}/org-clock.hs";
        };
        "pulseaudio" = {
           "scroll-step" = 1;
            "format" = "{volume}% {icon} {format_source}";
            "format-bluetooth" = "{volume}% {icon} {format_source}";
            "format-bluetooth-muted" = " {icon} {format_source}";
            "format-muted" = " {format_source}";
            "format-source" = "{volume}% ";
            "format-source-muted" = "";
            "format-icons" = {
              "headphone" = "";
              "hands-free" = "";
              "headset" = "";
              "phone" = "";
              "portable" = "";
              "car" = "";
              "default" = ["" "" ""];
            };
            "on-click" = "pavucontrol";
        };
      }];
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
        font = "${font} 12";
      };
    };
    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${dokumentoj}/Personal/.password-store";
      };
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
          "l" =  "mode-enter insert";
          "n" =  "scroll down";
          "e" =  "scroll up";
          "i" =  "scroll right";
          "j" =  "search-next";
          "b" =  "set-cmd-text -s :tab-select ";
          "gL" =  "open javascript:location.href='org-protocol://capture?template=l&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(document.getSelection())";
          "gM" =  "open javascript:location.href='org-protocol://roam-ref?template=m&ref='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(document.getSelection())";
          "gR" = "open javascript:location.href='org-protocol://roam-ref?template=r&ref='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)";
          "gB" = "spawn -m ${scripts}/downloadBook.py {url}";
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
        "wd" = "https://www.wikidata.org/w/index.php?search={}";
        "gs" =  "https://scholar.google.com/scholar?q={}";
        "b" =  "https://www.google.com/search?tbm=bks&q={}";
        "aw" =  "https://wiki.archlinux.org/?search={}";
        "o" =  "https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&query={}";
        "p" =  "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&query={}";
        "d" =  "https://en.wiktionary.org/wiki/{}";
        "v" =  "https://eo.wiktionary.org/wiki/{}";
        "melpa" =  "https://melpa.org/#/?q={}";
        "s" =  "http://stackoverflow.com/search?q={}";
        "ss" = "https://www.semanticscholar.org/search?q={}";
        "m" =  "https://maps.google.com/maps?q={}";
        "c" =  "https://clio.columbia.edu/quicksearch?q={}";
        "gh" =  "https://github.com/search?q={}&type=Repositories";
        "h" =  "https://hackage.haskell.org/packages/search?terms={}";
        "ho" = "https://hoogle.haskell.org/?hoogle={}";
        "libgen" =  "https://libgen.is/search.php?req={}";
        "viki" =  "https://eo.wikipedia.org/w/index.php?search={}";
        "ia" =  "https://archive.org/details/texts?and%5B%5D={}&sin=";
        "mm" =  "https://muse-jhu-edu.ezproxy.cul.columbia.edu/search?action=search&query=content:{}:and&limit=journal_id:131&min=1&max=10&t=search_journal_header";
      };
      settings = {
        content.local_content_can_access_remote_urls = true;
        content.headers.accept_language = "eo,en-US,en,fr";
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
          default_family = "${font}";
          default_size = "12pt";
        };
        hints.chars = "arstdhneio";
        url.default_page = "${scripts}/homepage/homepage.html";
      };
    };
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
  services = {
    # gnome-keyring.enable = true;
    gpg-agent.enable = true;
    clipmenu = {
      enable = true;
      launcher = "rofi";
    };
    dunst = {
      enable = true;
      settings = {
        global = {
          geometry = "950x80-30+70";
          padding = 32;
          horizontal_padding = 30;
          # frame_width = 10;
          font = "${font} 12";
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
      waylandDisplay = "eDP-1";
    };
    kanshi = {
      enable = true;
      profiles = {
        undocked = {
          outputs = [
            {
              criteria = "eDP-1";
            }
          ];
        };
        docked = {
          outputs = [
            { criteria = "eDP-1";
              position = "0,0";
            }
            {
              criteria = "Samsung Electric Company SAMSUNG 0x00000F00";
              position = "380,1440";
            }
          ];
        };
        clamshell = {
          outputs = [
            { criteria = "eDP-1";
              status = "disable";
            }
            {
              criteria = "Samsung Electric Company SAMSUNG 0x00000F00";
              position = "0,0";
            }
          ];
        };
      };
    };
    pueue = {
      enable = true;
      settings = {
        shared = {
          pueue_directory = "~/.local/share/pueue";
        };
        client = {};
        daemon = {
          default_parallel_tasks = 2;
        };
      };
    };
    screen-locker = {
      enable = false;
      lockCmd = "${lockCmd}";
    };
    swayidle = {
      enable = false;
      events = [{
        event = "before-sleep";
        command = "${lockCmd}";
      }];
      timeouts = [{ timeout = 900; command = "${lockCmd}"; }];
    };
  };
  qt = {
    enable = true;
    style = {
      name = "adwaita-dark";
      package = pkgs.adwaita-qt;
    };
    platformTheme.name = "adwaita";
  };

  # Dotfiles for the home root, ~/
  home = {
    # This should only be necessary with non-NixOS
    keyboard = {
      options = [ "caps:escape" "esperanto:colemak" ];
      variant = "colemak";
    };
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11.enable = true;
    };
    packages = with pkgs; [
      # neovim-nightly
      #
      pkgs.pass # This is necessary for protonmail-bridge
      pkgs.gnome.gnome-keyring # Same
    ];
    file = {
      ".config/doom" = {
      source = ./emacs/doom.d;
      recursive = true;
      onChange = "$HOME/.config/emacs/bin/doom sync";
    };
      ".local/share/applications/org-protocol.desktop".text = ''
[Desktop Entry]
Name=org-protocol
Comment=Intercept calls from emacsclient to trigger custom actions
Categories=Other;
Keywords=org-protocol;
Icon=emacs
Type=Application
Exec=emacsclient -- %u
Terminal=false
StartupWMClass=Emacs
MimeType=x-scheme-handler/org-protocol;'';
      #".emacs.d/init.el".text = ''
      #    (load "default.el")
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
    language = {
      base = "eo";
    };
    # sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    # sessionVariables.LC_ALL = "eo.UTF-8";
    username = "jon";
    stateVersion = "22.05";
  };

  systemd.user = {
    services = {
      # protonmail = {
      #   Unit = { Description = "Protonmail Bridge"; };
      #   Service.ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --noninteractive";
      #   Install.WantedBy = [ "default.target" ];
      # };
      dwall = {
        Unit = {
          Description = "Set dynamic wallpaper using Dwall";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${scripts}/dynamic-wallpaper/dwall.sh -p -s aurora";
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
  wayland.windowManager.sway = {
    enable = true;
    extraConfigEarly = ''include "${dots}/colors-sway"'';
    config = {
      bars = [];
      # colors = {
      #   focused = {
      #     background = "$color2";
      #     border = "$color2";
      #     text = "$foreground";
      #     indicator = "$color2";
      #     childBorder = "$color2";
      #   };
      #   focusedInactive = {
      #     background = "$color1";
      #     text = "$foreground";
      #     border = "$color1";
      #     indicator = "$color1";
      #     childBorder = "$color1";
      #   };
      #   unfocused = {
      #     background = "$color1";
      #     border = "$color2";
      #     text = "$foreground";
      #     indicator = "$color1";
      #     childBorder = "$color1";
      #   };
      # };
      fonts = { names = [ "Font Awesome" "${font}"]; size = 14.0;};
      gaps = { outer = 0; inner = 10; };
      input = {
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "colemak";
          xkb_options = "caps:escape,esperanto:colemak";
        };
        "type:touchpad" = {
          click_method = "clickfinger";
        };
      };
      output = { "eDP-1" = {
        # background = "$wallpaper fill";
      }; };
      left = "h";
      down = "n";
      up = "e";
      right = "i";
      modifier = "Mod4";
      keybindings =
        {
          "Mod4+j" = "workspace 1";
          "Mod4+l" = "workspace 2";
          "Mod4+u" = "workspace 3";
          "Mod4+y" = "workspace 4";
          "Mod4+Shift+j" = "move container to workspace number 1";
          "Mod4+Shift+l" = "move container to workspace number 2";
          "Mod4+Shift+u" = "move container to workspace number 3";
          "Mod4+Shift+y" = "move container to workspace number 4";
          "Mod4+h" = "exec alacritty";
          "Mod4+c" = "clipmenu";
          "Mod4+Shift+c" = "kill";
          "Mod4+space" = "exec ${pkgs.rofi}/bin/rofi -show drun";
          # "Mod4+y" = "output eDP-1 disable";
          # "Mod4+Shift+y" = "output eDP-1 enable";
          "Mod4+n" = "workspace next";
          "Mod4+e" = "workspace prev";
          "Mod4+Shift+q" = "exit";
          "Mod4+Shift+r" = "restart";
          "Mod4+p" = "focus parent";
          "Mod4+Shift+p" = "focus child";
          "Mod1+h" = "focus left";
          "Mod1+n" = "focus down";
          "Mod1+e" = "focus up";
          "Mod1+i" = "focus right";
          "Mod4+r" = "mode resize";
          "Mod4+g" = "mode gaps";
          "Mod4+o" = "exec emacsclient --eval '(org-clock-in-last)'";
          "Mod4+Shift+o" = "exec emacsclient --eval '(org-clock-out)'";
          "Mod4+Shift+e" = "exec emacsclient -c";
          "Mod1+Shift+h" = "move left";
          "Mod1+Shift+n" = "move down";
          "Mod1+Shift+e" = "move up";
          "Mod1+Shift+i" = "move right";
          "Mod4+s" = "move scratchpad";
          "Mod4+Shift+s" = "scratchpad show";
          "Mod4+f" = "fullscreen toggle";
          "Mod4+t" = "floating toggle";
          "Mod4+x" = "layout toggle all";
          "Mod4+v" = "split v";
          "Mod4+Shift+v" = "split h";
          "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '+10%'";
          "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '10%-'";
          "XF86AudioRaiseVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl up";
          "XF86AudioLowerVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl down";
          "XF86AudioMute" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl mute";
          # Open agenda with Super + A
          "Mod4+a" = "exec emacsclient -c -e '(org-agenda-list)(delete-other-windows)(org-agenda-day-view)'";
          "Mod4+m" = "exec emacsclient -c -e '(mu4e)(mu4e-update-mail-and-index)'";
          # lock screen with Super + L
          # "Mod4+l" = "exec ${lockCmd}";
          # Change wallpaper
          "Mod4+w" = "exec ${pkgs.pywal}/bin/wal -e -t -i /home/jon/Bildoj/Ekranfonoj -o ${../scripts/pywal-reload.sh}";
          "Mod4+Shift+w" = "exec ${pkgs.pywal}/bin/wal -e -t -i /run/media/jon/systemrestore/.systemrestore/Bildoj/ -o ${../scripts/pywal-reload.sh}";
        };
      modes = {
        resize = {
          h = "resize shrink width 2";
          n = "resize grow height 2";
          e = "resize shrink height 2";
          i = "resize grow width 2";
          Escape = "mode default";
        };
        gaps = {
          h = "gaps inner current set 0";
          n = "gaps inner current minus 5";
          e = "gaps inner current plus 5";
          i = "gaps inner current set 50";
          Escape = "mode default";
        };
      };
      startup = [
        # Fix for slow GTK applications; see https://github.com/swaywm/sway/wiki#gtk-applications-take-20-seconds-to-start
        { command = "exec systemctl --user import-environment"; }
        # { command = "exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK"; }
        { command = "exec swaybg -i ~/Bildujo/Ekranfonoj/yz6ggt7m18l41.png -o '*' -m fill"; }
        { command = "exec ${pkgs.pywal}/bin/wal --theme base16-nord"; }
        { command = "exec megasync"; }
        { command = "exec waybar"; }
        { command = "exec ${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"; }
        { command = "exec ${pkgs.autotiling}/bin/autotiling"; }
      ];
      window.border = 10;
    };
    extraConfig = ''
      # Clamshell mode
      set $laptop eDP-1
      bindswitch --reload --locked lid:on output $laptop disable
      bindswitch --reload --locked lid:off output $laptop enable
      exec_always ${scripts}/sway-reload.sh
      # Touchpad gestures
      bindgesture swipe:right workspace prev
      bindgesture swipe:left workspace next
      bindgesture pinch:inward+up move up
      bindgesture pinch:inward+down move down
      bindgesture pinch:inward+left move left
      bindgesture pinch:inward+right move right
    '';
  };
  # Dotfiles for ~/.config, ~/.local/share, etc.
  xdg = {
    enable = true;
    configFile = {
      "waybar/style.css" = {
        source = ./waybar-style.css;
      };
    };
    dataFile = {
      "qutebrowser/userscripts/" = {
        source = ../scripts/qutebrowser-userscripts;
        recursive = true;
      };
    };
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = "org.gnome.Evince.desktop";
        "text/html" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
        "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
      };
    };
  };
}
