{ config, pkgs, lib, ... }:


{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  # disabledModules = [ "targets/darwin/linkapps.nix" ];
  home = {
    username = "jon";
    file.".inputrc".text = ''
        set editing-mode vi
    '';
    file."/Users/jon/Library/Application Support/xbar/plugins/org-clock.1m.sh" = {
      source = ../scripts/org-clock.sh;
      executable = true;
    };
    # homeDirectory = "/Users/jon";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "22.11";
    packages = with pkgs; [
      # Basic necessities
      coreutils
      curl
      wget

      # Minimal computing
      pass
      fd
      ripgrep
      bat
      tree

      # Scala
      sbt
      dotty
      ammonite
      metals
      # spark
      scalafmt
      scalafix

      # Dev
      direnv

      # Spark

      # Python
      pipenv
      poetry
      asdf
     (python3.withPackages(ps: with ps; [
       pandas
       plotly
       matplotlib
       python-lsp-server # Spacemacs integration
       flake8 # Syntax checking for emacs
       # scikit-learn
       altair
       # vega
       # vega_datasets
       jupyter
       jupyterlab
       # jupytext
       # tensorflow
       nltk
       pip
       pyarrow
       pytest
       pytest-cov
       pytest-watch
       numpy
       nose
       tldextract # required by qute-pass
     ]))

      # Fancy MacOS terminal
      # iterm2
      alacritty

      yabai  # Window manager
      skhd   # Hotkeys for window manager

      # Virtualization
      # utm # Doesn't work?
      # firefox
    ];
    # Hack for getting apps to show up in Spotlight, etc.
    # See https://github.com/nix-community/home-manager/issues/1341
    # activation = lib.mkIf pkgs.stdenv.isDarwin {
    #   copyApplications = let
    #     apps = pkgs.buildEnv {
    #       name = "home-manager-applications";
    #       paths = config.home.packages;
    #       pathsToLink = "/Applications";
    #     };
    #   in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    #       baseDir="$HOME/Applications/Home Manager Apps"
    #       if [ -d "$baseDir" ]; then
    #         rm -rf "$baseDir"
    #       fi
    #       mkdir -p "$baseDir"
    #       for appFile in ${apps}/Applications/*; do
    #         target="$baseDir/$(basename "$appFile")"
    #         $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
    #         $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
    #       done
    #   '';
    # };
  };
  # Let Home Manager install and manage itself.
  programs = {
    alacritty = {
      enable = true;
      settings = {
        window = {
          opacity = 0.9;
          padding = {
            x = 20;
            y = 20;
          };
        };
        font = {
          size = 12;
          normal.family = "Monaco";
        };
      };
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    home-manager.enable = true;
    emacs.enable = true;
    git = {
      enable = true;
      userName = "Jonathan Reeve";
      userEmail = "j_reeve@apple.com";
      extraConfig = {
        core.editor = "emacsclient -c";
        pull.rebase = false;
      };
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
    nushell = {
      enable = true;
      configFile.text = '''';
      envFile.text = ''
        let-env STARSHIP_SHELL = "nushell"
        let-env PATH = ($env.PATH | split row (char esep) | prepend '/usr/local/bin')
      '';
      # let-env PATH = ($env.PATH | prepend '/opt/homebrew/bin')
      # let-env PATH = ($env.PATH | prepend '/Users/jon/.nix-profile/bin')
      # let-env PATH = ($env.PATH | prepend '/nix/var/nix/profiles/default/bin')
      # let-env PATH = ($env.PATH | prepend '/System/Cryptexes/App/usr/bin')
    };
    qutebrowser = {
      enable = true;
      package = pkgs.hello;
      extraConfig = ''
        c.statusbar.padding = {'top': 5, 'bottom': 5, 'left': 3, 'right': 3}
        c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 2, 'right': 2}
        c.content.user_stylesheets = [ "~/Dotfiles/dotfiles/custom.css" ]
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
        "viki" =  "https://eo.wikipedia.org/w/index.php?search={}";
        "ia" =  "https://archive.org/details/texts?and%5B%5D={}&sin=";
        "mm" =  "https://muse-jhu-edu.ezproxy.cul.columbia.edu/search?action=search&query=content:{}:and&limit=journal_id:131&min=1&max=10&t=search_journal_header";
      };
      settings = {
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
          # completion.category = "11pt monospace";
          # default_family = "${font}, Terminus, Monospace, monospace, Fixed";
          default_size = "12pt";
        };
        hints.chars = "arstdhneio";
      };
    };
    starship = {
      enable = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      enableVteIntegration = true;
      autocd = true;
      defaultKeymap = "viins";
      initExtra = ''
        export JAVA_HOME=`/usr/libexec/java_home -v11`
        export PATH="$HOME/.config/emacs/bin:$JAVA_HOME/bin:/opt/homebrew/bin:$PATH"
        export PATH="$PATH:/Users/jon/Library/Application Support/Coursier/bin"
        # This should automatically work, except it is disabled for emacs
        eval "$(/etc/profiles/per-user/jon/bin/starship init zsh)"
        export PATH="$JAVA_HOME/bin:/opt/homebrew/bin:$PATH"
        alias ls="ls --color"
        alias git-root='cd $(git rev-parse --show-cdup)'
        alias proxy='ssh -vND localhost:7999 pvgateway'
	alias upgrade='darwin-rebuild switch --flake ~/Dotfiles/dotfiles'
      '';
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
  };
  xdg = {
    enable = true;
    configFile."skhd/skhdrc".text = ''
      lalt - t : yabai -m window --toggle float
      lalt - h : yabai -m window --focus west
      lalt - n : yabai -m window --focus south
      lalt - e : yabai -m window --focus north
      lalt - i : yabai -m window --focus east

      shift + lalt - h : yabai -m window --swap west
      shift + lalt - n : yabai -m window --swap south
      shift + lalt - e  : yabai -m window --swap north
      shift + lalt - i : yabai -m window --swap east
    '';
    configFile."yabai/yabairc" = {
      text = ''
        yabai -m config layout float

        yabai -m config top_padding    20
        yabai -m config bottom_padding 20
        yabai -m config left_padding   20
        yabai -m config right_padding  20
        yabai -m config window_gap     20

        yabai -m rule --add app=Emacs manage=on

        # yabai -m config window_border on
        # yabai -m config window_border_width 6
        # yabai -m config active_window_border_color 0xff775759
        # yabai -m config normal_window_border_color 0xff555555

        # yabai -m window --toggle border
        # yabai -m rule --add app=Terminal border=off

        # Scripting addition
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
        sudo yabai --load-sa
      '';
      executable = true;
    };
    configFile."doom" = {
        source = ./doom;
        recursive = true;
        onChange = "$HOME/.config/emacs/bin/doom sync";
    };

  };
}
