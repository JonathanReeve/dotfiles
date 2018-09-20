# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    plymouth.enable = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "jon-laptop"; # Define your hostname.
    networkmanager.enable = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
      #enableWideVine = true;
    };
  };

  # Select internationalisation properties.
  i18n = {
    # HiDPI Font
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  # Fonts!
  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    font-awesome-ttf
  ];
  # Set your time zone.
  time.timeZone = "America/New_York";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     # Nix stuff
     nix-index              # Indexing files for nix-locate
     nix-prefetch-git nix-prefetch-scripts # Help writing .nix files
     cabal2nix              # Haskell packages to .nix expressions
     pypi2nix
     home-manager           # Dotfiles management
     # CLI
     fish                   # Shell
     vim emacs              # Text editor
     pass encfs             # Passwords and encryption
     aspell aspellDicts.en  # Spell checker
     xorg.xbacklight        # Brightness control
     networkmanager
     gcc gnumake
     gnupg
     wget
     isync mu w3m           # Mail
     pandoc                 # Document manipulation
     haskellPackages.pandoc-citeproc
     #haskellPackages.pandoc-crossref
     # This might be the way to disable tests for pandoc-crossref, but I can't get it to work yet.
     # Suggested here: https://github.com/lierdakil/pandoc-crossref/issues/199#issuecomment-422594460
     # pandoc-crossref = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.pandoc-crossref (oldAttrs: {
     #   doCheck = false;
     # });
     tectonic               # Latex
     texlive.combined.scheme-context
     git                    # Version control
     dropbox-cli
     # Haskell Development
     stack
     ghc
     haskellPackages.turtle
     libxml2
     sqlite sqlite-interactive # Sqlite
     # Python Development
     (python3.withPackages(ps: with ps; [
       pandas
       jupyter
       xonsh
       virtualenvwrapper
     ]))
     # Elm
     # elmPackages.elm
     # Minimal computing
     ranger highlight       # File manager
     scrot                  # Screenshots
     # rofi                   # Launcher
     zathura                # PDF Viewer
     polybar                # System monitor, etc.
     compton                # Compositor
     mpv                    # Minimalist video player
     termite                # Vim-like modal terminal
     pywal                  # Wallpapers
     feh                    # Display imaes
     bspwm sxhkd            # Window manager
     dunst libnotify        # Notifications
     weechat                # IRC
     fzf                    # Fuzzy file finder
     ag                     # Fast grep replacement
     bat                    # Cat replacement
     fd                     # Find replacement
     i3lock-fancy           # Screen locker
     ncdu                   # Fancy disk usage analyzer
     # GUI
     qutebrowser            # Web browser
     chromium               # Another web browser
     zotero
     numix-cursor-theme
     # KDE
     konversation           # IRC
     gwenview               # Image viewer
     okular                 # PDF viewer
     dolphin                # File manager
     kate                   # Text editor
     ark                    # Archive management
     spectacle              # Screenshots
     #dragon                 # Video player
   ];

  environment.variables = {
    # HiDPI Settings
    PLASMA_USE_QT_SCALING = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_QPA_PLATFORM = "xcb";
    QT_QPA_PLATFORMTHEME = "qt5ct";
    QT_QUICK_CONTROLS_STYLE = "org.kde.desktop";
    QT_SCREEN_SCALE_FACTORS = "eDP1=2;HDMI1=2;VIRTUAL1=2;";
    XCURSOR_THEME = "breeze_cursors";
    XCURSOR_SIZE = "48";

    # Preferred applications
    EDITOR = "emacsclient -c";
    BROWSER = "qutebrowser";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services = {
    # Enable emacs daemon, and set EDITOR to emacsclient
    emacs = {
      enable = true;
      defaultEditor = true;
    };

    # Power button invokes suspend, not shutdown.
    logind.extraConfig = "HandlePowerKey=suspend";

    # X
    xserver = {
      enable = true;
      dpi = 180;
      # Enable touchpad support.
      libinput.enable = true;
      # Keyboard settings
      layout = "us,us";
      xkbVariant = "altgr-intl,colemak";
      # Change layouts with Alt+Space
      xkbOptions = "grp:alt_space_toggle";
      # Enable the KDE Desktop Environment.
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
      #windowManager.bspwm.enable = true;
      desktopManager.session = [{
        name = "home-manager";
        start = ''
          ${pkgs.stdenv.shell} $HOME/.xsession-hm &
          waitPID=$!
        '';
      }];
    };
  };

  # Shell
  programs = {
    fish.enable = true;
    gnupg.agent = { enable = true; enableSSHSupport = true; };
  };

  users.users = {
    jon =
      { isNormalUser = true;
        home = "/home/jon";
        shell = pkgs.fish;
        description = "Jonathan Reeve";
        extraGroups = [ "wheel" "networkmanager" "tty" "dialout"];
      };
    systemrestore =
      { isNormalUser = true;
        home = "/home/systemrestore";
        shell = pkgs.fish;
        description = "System Restore";
        extraGroups = [ "wheel" "networkmanager" ];
      };
  };

  # Don't ask for my password *quite* as often.
  security.sudo.extraConfig = "Defaults timestamp_timeout=60";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
