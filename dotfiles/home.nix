{ config, pkgs, lib, ... }:


{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  # disabledModules = [ "targets/darwin/linkapps.nix" ];
  # imports = [ ./copy-apps.nix ];
  home = {
    username = "jon";
    file.".inputrc".text = ''
        set editing-mode vi
    '';
    file."/Users/jon/Library/Application Support/xbar/plugins/org-clock.1m.sh" = {
      source = ../scripts/org-clock.sh;
      executable = true;
    };
    homeDirectory = "/Users/jon";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    shell = {
      enableNushellIntegration = true;
      enableShellIntegration = true;
    };

    stateVersion = "22.11";
    packages = with pkgs; [
      # Basic necessities
      coreutils
      cmake
      # curl
      wget
      lftp
      gh
      bzip3

      # Minimal computing
      pass
      fd
      ripgrep
      bat
      tree
      jq
      jc
      pandoc

      # Julia
      julia-bin

      # Scala
      sbt
      dotty
      ammonite
      metals
      # spark
      scalafmt
      scalafix

      # Java
      ant
      neo4j

      # Clojure
      clojure
      clojure-lsp
      leiningen
      babashka
      exercism

      # Dev
      direnv
      # devenv
      fh

      # Rust
      rustup

      # Spark

      # Python
      pipenv
      poetry
      pyright

      asdf
      poppler
      rye
     (python3.withPackages(ps: with ps; [
     #   # LSP Integration
     #   # python-lsp-server
     #   # pylsp-mypy
     #   # flake8
     #   # For like, you know, science
       pandas
       plotly
       numpy
       scipy
     #   # scikit-learn
     #   # matplotlib
     #   # jupyter
     #   # jupyterlab
     #   # # tensorflow
     #   # nltk
       # pip
       awscli
     ]))
      graphviz

      # Haskell
      cabal2nix
      cabal-install
      haskell-language-server

      (haskellPackages.ghcWithPackages (ps: with ps; [
        hlint
        cabal-fmt
        cabal-gild
      ]))

      awscli

      # Fancy MacOS terminal
      # iterm2
      alacritty

      #
      # nyxt
      yabai  # Window manager
      skhd   # Hotkeys for window manager

      #elixir

      # Virtualization
      # utm # Doesn't work?
      # firefox

      # elm stuff
      #elm
      # elmPackages.elm
      # elmPackages.lamdera

      nodejs

      # Latex etc
      # texlive.combined.scheme-full
      ispell
    ];
  };
  programs = {
    alacritty = {
      enable = false;
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
    atuin = {
      enable = false;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
    carapace = {
      enable = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = true;
    };
    doom-emacs = {
      enable = true;
      doomDir = ./doom;
      doomLocalDir = "${config.home.homeDirectory}/.local/share/nix-doom";
      extraPackages = epkgs: with epkgs; [
        treesit-grammars.with-all-grammars
        nushell-mode
        ob-nushell
        org-node
        lispy
        treemacs
        treemacs-nerd-icons
        nerd-icons
      ];
    };
    home-manager.enable = true;
    # emacs = {
    #   enable = true;
    #   extraPackages = epkgs: [ epkgs.pdf-tools ];
    # };
    git = {
      enable = true;
      lfs.enable = true;
      userName = "Jonathan Reeve";
      userEmail = "j_reeve@apple.com";
      extraConfig = {
        core.editor = "emacsclient -c";
        pull.rebase = false;
        http.postBuffer = 157286400;
      };
    };
    go = {
      enable = true;
    };
    gpg.enable = true;
    neovim = {
      enable = true;
      # package = pkgs.neovim;
      # plugins = with pkgs.vimPlugins; [ ];
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
      configFile.text = ''
        $env.config = {
          edit_mode: vi
          keybindings: [
            {
              name: enter_normal_mode
              modifier: control
              keycode: char_q
              mode: vi_insert
              event: { send: esc }
            }
          ]
        }
        use std/util "path add"
        path add "~/.local/bin"
        path add "/usr/local/bin"
      '';
      shellAliases = {
        # upgrade = "nix flake update --flake ~/Dotfiles/dotfiles/; sudo darwin-rebuild switch --flake ~/Dotfiles/dotfiles";
        git-root = "cd (git rev-parse --show-cdup)";
        proxy = "ssh -vND localhost:7999 pvgateway";
        gst = "git status";
        gcm = "git commit -m";
      };
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
    vscode = {
      enable = false;
      # extensions = with pkgs.vscode-extensions; [
      #   # foam.foam-vscode
      #   asvetliakov.vscode-neovim
      #   ms-python.python
      #   scala-lang.scala
      #   scalameta.metals
      #   vscjava.vscode-gradle
      #   # vscjava.vscode-java-pack
      # ];
    };
    zsh = {
      enable = true;
      autosuggestion.enable = true;
      enableCompletion = true;
      syntaxHighlighting = {
        enable = true;
      };
      enableVteIntegration = true;
      autocd = true;
      defaultKeymap = "viins";
      initContent = ''
export JAVA_HOME=`/usr/libexec/java_home -v11`
export PATH="$HOME/.config/emacs/bin:$HOME/.local/bin:$JAVA_HOME/bin:/opt/homebrew/bin:$PATH"
export PATH="$PATH:/Users/jon/Library/Application Support/Coursier/bin"
# This should automatically work, except it is disabled for emacs
eval "$(/etc/profiles/per-user/jon/bin/starship init zsh)"
export PATH="$JAVA_HOME/bin:/opt/homebrew/bin:$PATH"
alias ls="ls --color"
alias git-root='cd $(git rev-parse --show-cdup)'
alias proxy='ssh -vND localhost:7999 pvgateway'
#alias upgrade='sudo darwin-rebuild switch --flake ~/Dotfiles/dotfiles'

source ~/Dotfiles/scripts/vterm.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
      '';
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = true;
    };
  };
  xdg = {
    enable = true;
    configFile."nushell/emacs-config.nu".text = ''
      source ~/.config/nushell/config.nu
      source ~/Dotfiles/scripts/vterm.nu
    '';
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
        # onChange = "$HOME/.config/emacs/bin/doom sync";
    };

  };
}
