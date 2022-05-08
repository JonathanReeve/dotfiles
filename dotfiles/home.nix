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
        # neomutt.enable = true;
        # notmuch.enable = true;
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
        # neomutt.enable = true;
        # notmuch.enable = true;  #
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
  programs = {
    alacritty = {
      enable = true;
      settings = {
        font.normal.family = "${font}";
        font.size = 9;
        window.opacity = 0.1;
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
    neomutt = {
      enable = false;
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
         "man" = "env PAGER=\"vim -R -c 'set ft=man'\" man";
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
              cat ~/.cache/wal/sequences
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
      # settings = { add_newline = false;
      #              character = { format = "$symbol "; };
      #            };
    };
    zoxide = {
      enable = true;
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
    };
    waybar = {
      enable = true;
      settings = [ {
        mainBar = {
          layer = "top";
          position = "top";
          height = 30;
          output = [ "eDP-1" "DP-1-1" ];
          modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
          modules-center = [ "sway/window" ];
          modules-right = [ ];
          modules = {
          };
        };
      } ];
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
        PASSWORD_STORE_DIR = "/home/jon/Dokumentujo/Personal/.password-store";
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
    xsuspender = {
      enable = true;
      defaults = {
        downclockOnBattery = 1;
      };
    };
    clipmenu.enable = true;
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
    };
    picom = {
      enable = true;
      experimentalBackends = true;
      fade = true;
      fadeDelta = 5;
      blur = true;
      shadow = true;
      # TODO replace with https://github.com/ibhagwan/picom
      package = pkgs.picom.overrideAttrs (old: rec {
        src = pkgs.fetchFromGitHub {
          owner = "jonaburg"; repo = "picom";
          rev = "a8445684fe18946604848efb73ace9457b29bf80";
          sha256 = "R+YUGBrLst6CpUgG9VCwaZ+LiBSDWTp0TLt1Ou4xmpQ=";
          fetchSubmodules = true;
        };
      });
      extraOptions = ''
        # corner-radius: 10.0;
        blur: {
          method = "dual_kawase";
          strength = 5;
          background = false;
          background-frame = false;
          background-fixed = false;
        }
      '';
      vSync = true;
    };
    polybar = {
      enable = true;
      package = pkgs.polybarFull;
      script = "/usr/bin/env polybar main_bar &";
      config = {
        "bar/ext_bar" = {
           monitor = "DP-1-1";
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
           font-0 = "${font}:size=14;1";
           font-1 = "Font Awesome 5 Free:size=11:style=Solid;1";
           font-2 = "Noto Sans Symbols:size=11;1";
           modules-left = "i3 xwindow";
           modules-center = "date";
           modules-right = "org-clock volume backlight filesystem memory cpu battery network";
        };
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
           font-0 = "${font}:size=14;1";
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
           ramp-capacity-0 = "%{F#f00}%{F-}";
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
      sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      sessionVariables.LC_ALL = "eo.UTF-8";
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
  wayland = {
    windowManager.sway = {
      enable = true;
      extraConfig = "include \"$HOME/.cache/wal/colors-sway\"";
      config = {
        bars = [];
        fonts = { names = [ "Font Awesome" "${font}"]; size = 14.0;};
        gaps = { outer = 10; inner = 10; };
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
        left = "h";
        down = "n";
        up = "e";
        right = "i";
        modifier = "Mod4";
        keybindings =
          {
            "Mod4+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
            "Mod4+Shift+c" = "kill";
            "Mod4+space" = "exec ${pkgs.ulauncher}/bin/ulauncher";
            "Mod4+n" = "workspace next";
            "Mod4+e" = "workspace prev";
            "Mod4+Shift+q" = "exec sway-msg exit";
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
            # "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '+10%'";
            # "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set '10%-'";
            # "XF86AudioRaiseVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl up";
            # "XF86AudioLowerVolume" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl down";
            # "XF86AudioMute" =  "exec --no-startup-id ${pkgs.pulseaudio-ctl}/bin/pulseaudio-ctl mute";
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
          # { command = "xrdb -merge ~/.cache/wal/colors.Xresources"; }
          # { command = "setxkbmap -layout us -variant colemak -option caps:escape -option esperanto:colemak"; }
          # { command = "exec systemctl --user import-environment"; always = true; notification = false; }
        ];
        window.border = 10;
      };
    };
  };
  xsession = {
    enable = true;
    scriptPath = ".xsession-hm";
    # pointerCursor = {
    #   name = "Vanilla-DMZ";
    #   package = pkgs.vanilla-dmz;
    # };
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
          floating.criteria = [
            { class = "^MEGAsync$"; }
            { class = "zoom"; }
          ];
          bars = [];
          fonts = { names = [ "Font Awesome" "${font}"];
                    size = 14.0;
                  };
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
              "Mod4+Shift+q" = "exec i3-msg exit";
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
              "Mod4+c" = "exec clipmenu";
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
            { command = "${pkgs.pywal}/bin/wal -R"; notification = false; }
            # { command = "megasync"; notification = false; }
            { command = "xrdb -merge ~/.cache/wal/colors.Xresources"; notification = false; }
            { command = "setxkbmap -layout us -variant colemak -option caps:escape -option esperanto:colemak"; }
            # { command = "exec systemctl --user import-environment"; always = true; notification = false; }
            { command = "exec systemctl --user restart polybar"; always = true; notification = false; }
            # { command = "${pkgs.gnome3.gnome_settings_daemon}/libexec/gsd-xsettings"; }
          ];
          window.border = 10;
        };
    };
    windowManager.xmonad = {
      enable = false;
      enableContribAndExtras = true;
      config = "${dots}/xmonad.hs";
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.monad-logger
      ];


    };
  };
  # Dotfiles for ~/.config, ~/.local/share, etc.
  xdg = {
    enable = true;
    dataFile = {
      "qutebrowser/userscripts/" = {
        source = ../scripts/qutebrowser-userscripts;
        recursive = true;
      };
    };
  };
}
