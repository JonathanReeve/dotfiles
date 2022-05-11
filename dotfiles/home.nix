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
  # font = "Monoid";
  # font = "Nova Mono";
  font = "Fantasque Sans Mono";
  backgroundColor = "#243442"; # Blue steel
  foregroundColor = "#deedf9"; # Light blue
  warningColor = "#e23131"; # Reddish
  lockCmd = "${pkgs.i3lock-blur}/bin/i3lock-blur";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
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
        passwordCommand = "${pkgs.gopass}/bin/gopass show 127.0.0.1:1143/jonathan@jonreeve.com";
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
        # notmuch.enable = true;
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
      };
    };
  };
  fonts.fontconfig.enable = true;
  programs = {
    alacritty = {
      enable = true;
      package = pkgs.hello; # Don't actually install it
      settings = {
        font.normal.family = "${font}";
        font.size = 16;
        window.opacity = 0.9;
        colors.transparent_background_colors = true;
      };
    };
    bottom.enable = true;
    broot = {
      enable = false;
      enableFishIntegration = true;
      modal = true;
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
    };
    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
      extraConfig = {
        core.editor = "emacsclient -c";
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
            nixos-rebuild --use-remote-sudo switch --flake ${dots}
            '';
         clean = "nix-store --gc --print-roots; and sudo nix-collect-garbage --delete-older-than 5d";
         # A function for renaming the most recent PDF, and putting it in my Papers dir.
         rename-pdf="mv (ls -t /tmp/*.pdf | head -n 1) ~/Dokumentujo/Papers/$argv.pdf";
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
              # cat ~/.cache/wal/sequences
            end

            set -U vaultmount ~/.private-mount
            set -U vaultloc ~/Dokumentujo/Personal/.Vault_encfs

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
    };
    nushell = {
      enable = true;
      settings = {
        edit_mode = "vi";
        startup = [ "def em [f] {emacsclient -c $f &; disown}"
                    "def wal-fav [] {open ~/.cache/wal/colors.json | get wallpaper | each { echo $it (char newline)} | str collect | save -a ~/.cache/wal/favs}"
                    "def wal-fav-set [] {wal -i (open ~/.cache/wal/favs | lines | shuffle | keep 1)}"
                    "def wal-recent [] {wal -i (ls /run/media/jon/systemrestore/.systemrestore/Bildoj | where type == 'File' | sort-by modified -r | keep 50 | shuffle | keep 1 | get name)}"
                    "def wal-backup [] {sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore}"
                    "let scripts = ~/Dotfiles/scripts"
                    "def proj [project] {
                        open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { qutebrowser $it.websites };
                        open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
                    }"
                    "zoxide init nushell --hook prompt | save ~/.zoxide.nu"
                    "source ~/.zoxide.nu"
                  ];
        prompt = "echo (starship prompt)";
        env = { STARSHIP_SHELL = "nushell"; };
      };
    };

    starship = {
      enable = true;
    };
    zoxide = {
      enable = true;
    };
    foot = {
      enable = true;
      settings = {
        main = {
          term = "xterm-256color";
          font = "${font}:size=8";
        };
        colors.alpha = 0.8;
      };
    };
    rofi = {
      enable = true;
      theme = "~/.cache/wal/colors-rofi-dark.rasi";
      font = "${font} 12";
    };
    waybar = {
      enable = true;
      settings = [{
          layer = "top";
          height = 30;
          modules-left = [ "sway/workspaces" "sway/mode" "sway/window" ];
          modules-center = [ "custom/clock" ];
          modules-right = [ "custom/org-clock" "cpu" "memory" "network" "battery" "clock" ];
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
            max-length = 40;
            interval = "30s";
            exec = "/bin/bash ${scripts}/org-clock.sh";
          };
      }];
    };
    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "/home/jon/Dokumentujo/Personal/.password-store";
      };
    };
    qutebrowser = {
      enable = true;
      package = pkgs.hello; # Don't install qutebrowser here, since it doesn't work .
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
        "gB" = "spawn -m ~/Dotfiles/scripts/downloadBook.py {url}";
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
        content.headers.accept_language = "eo,en-US,en,fr";
        content.pdfjs = true;
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
          default_size = "12pt";
        };
        hints.chars = "arstdhneio";
        url.default_page = "${scripts}/homepage/homepage.html";
      };
    };
    vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        ms-python.python
        vscodevim.vim
        ms-vsliveshare.vsliveshare
      ];
      haskell = {
        enable = true;
        # THis says it needs a special overlay of some sort, so disabling
        hie.enable = false;
      };
    };
  };
  services = {
    # gnome-keyring.enable = true;
    gpg-agent.enable = true;
    clipmenu.enable = true;
  };
  gtk = {
    enable = false;
    # theme = {
    #   package = pkgs.adwaita-gtk;
    #   name = "Breeze-Dark";
    # };
    # iconTheme = {
    #   package = pkgs.breeze-icons;
    #   name = "breeze-dark";
    # };
    # Give Termite some internal spacing.
    gtk3.extraCss = ".termite {padding: 10px;}";
  };
  qt = {
    enable = false;
    style = {
      name = "adwaita-dark";
      package = pkgs.adwaita-qt;
    };
    platformTheme = "gnome";
  };

  # Dotfiles for the home root, ~/
  home = {
      # This should only be necessary with non-NixOS
      keyboard = {
        options = [ "caps:escape" "esperanto:colemak" ];
        variant = "colemak";
      };
      packages = with pkgs; [
        # neovim-nightly
        #
        pkgs.pass # This is necessary for protonmail-bridge
        pkgs.gnome.gnome-keyring # Same
        pkgs.fantasque-sans-mono
        pkgs.protonmail-bridge
      ];
      file = {
        ".doom.d/" = {
          source = ./emacs/doom.d;
          recursive = true;
          onChange = "$HOME/.emacs.d/bin/doom sync";
        };
        ".local/share/applications/org-protocol.desktop".text = ''
          [Desktop Entry]
          Name=Org-Protocol
          Exec=emacsclient %u
          Icon=emacs-icon
          Type=Application
          Terminal=false
          MimeType=x-scheme-handler/org-protocol
        '';
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
      sessionVariables = {
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        LC_ALL = "eo.UTF-8";
        PASSWORD_STORE_DIR = "/home/jon/Dokumentujo/Personal/.password-store";
      };

  };

  systemd.user = {
    services = {
      protonmail = {
        Unit = { Description = "Protonmail Bridge"; };
        Service.ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --noninteractive";
        Install.WantedBy = [ "default.target" ];
      };
    };
    timers = {
    };
  };
  targets.genericLinux.enable = true;
  wayland = {
    windowManager.sway = {
      enable = true;
      config = {
        bars = [];
        colors = {};
        fonts = { names = [ "Font Awesome" "${font}"]; size = 14.0;};
        gaps = { outer = 10; inner = 10; };
        input = { "*" = {
          xkb_layout = "us";
          xkb_variant = "colemak";
          xkb_options = "caps:escape,esperanto:colemak";
        }; };
        left = "h";
        down = "n";
        up = "e";
        right = "i";
        modifier = "Mod4";
        keybindings =
          {
            "Mod4+1" = "workspace 1";
            "Mod4+2" = "workspace 2";
            "Mod4+3" = "workspace 3";
            "Mod4+4" = "workspace 4";
            "Mod4+5" = "workspace 5";
            "Mod4+Shift+1" = "move container to workspace number 1";
            "Mod4+Shift+2" = "move container to workspace number 2";
            "Mod4+Shift+3" = "move container to workspace number 3";
            "Mod4+Shift+4" = "move container to workspace number 4";
            "Mod4+Shift+5" = "move container to workspace number 5";
            "Mod4+Return" = "exec alacritty";
            "Mod4+Shift+c" = "kill";
            "Mod4+space" = "exec ${pkgs.ulauncher}/bin/ulauncher";
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
            "Mod4+o" = "exec emacsclient --eval '(org-clock-in-last)'";
            "Mod4+Shift+o" = "exec emacsclient --eval '(org-clock-out)'";
            "Mod4+Shift+e" = "exec emacsclient -c";
            "Mod1+Shift+h" = "move left";
            "Mod1+Shift+n" = "move down";
            "Mod1+Shift+e" = "move up";
            "Mod1+Shift+i" = "move right";
            "Mod4+s" = "move scratchpad";
            "Mod4+Shift+s" = "scratchpad show";
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
            "Mod4+l" = "exec ${lockCmd}";
            # Change wallpaper
            "Mod4+w" = "exec ${pkgs.pywal}/bin/wal -i /home/jon/Bildujo/Ekranfonoj -o ${../scripts/pywal-reload.sh}";
            "Mod4+Shift+w" = "exec ${pkgs.pywal}/bin/wal -i /run/media/jon/systemrestore/.systemrestore/Bildoj/ -o ${../scripts/pywal-reload.sh}";
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
          { command = "${pkgs.pywal}/bin/wal -R"; }
          { command = "megasync"; }
          { command = "waybar"; }
        ];
        window.border = 10;
      };

    };
  };
  xdg = {
    enable = true;
    dataFile = {
      "qutebrowser/userscripts/" = {
        source = ../scripts/qutebrowser-userscripts;
        recursive = true;
      };
    };
    configFile = {
      "waybar/style.css" = {
        source = ./waybar-style.css;
      };
    };
  };
}
