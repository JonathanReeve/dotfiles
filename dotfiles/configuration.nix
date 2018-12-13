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
    blacklistedKernelModules = [ "ideapad_laptop" ]; 
    cleanTmpDir = true;
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
  };

  # Select internationalisation properties.
  i18n = {
    consoleUseXkbConfig = true;
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
     cabal2nix pypi2nix
     # nodePackages.node2nix 
     home-manager           # Dotfiles management
     # CLI
     fish xonsh             # Shell
     vim emacs              # Text editor
     pass encfs             # Passwords and encryption
     aspell aspellDicts.en  # Spell checker
     xorg.xbacklight        # Brightness control
     networkmanager
     gcc gnumake
     gnupg
     wget
     isync mu w3m           # Mail
     haskellPackages.pandoc # Document manipulation
     haskellPackages.pandoc-citeproc
     #haskellPackages.pandoc-crossref
     haskellPackages.hlint
     haskellPackages.apply-refact
     haskellPackages.stylish-haskell
     haskellPackages.hasktags
     haskellPackages.hoogle
     libfprint fprintd      # Fingerprint login
     iio-sensor-proxy       # Accelerometer, gyroscope, etc.
     #tectonic               # Latex
     texlive.combined.scheme-full
     git                    # Version control
     dropbox-cli
     unzip                  # Archives
     # Haskell Development
     # stack
     ghc
     haskellPackages.turtle
     libxml2
     sqlite sqlite-interactive # Sqlite
     # Python Development
     (python3.withPackages(ps: with ps; [
       pandas
       matplotlib
       jupyter
       nltk
       # numpy
       # scikitlearn
       # textblob
       virtualenvwrapper
     ]))
     # Elm
     # elmPackages.elm
     # Minimal computing
     # ranger highlight       # File manager
     # scrot                  # Screenshots
     tree                   # Show file hierarchies
     lftp                   # Fast file transfers
     autojump               # Jump around! With `j`
     # rofi                   # Launcher
     # zathura                # PDF Viewer
     # polybar                # System monitor, etc.
     # compton                # Compositor
     # mpv                    # Minimalist video player
     # termite                # Vim-like modal terminal
     # pywal                  # Wallpapers
     # feh                    # Display imaes
     # bspwm sxhkd            # Window manager
     # dunst libnotify        # Notifications
     weechat                # IRC
     fzf                    # Fuzzy file finder
     ag                     # Fast grep replacement
     #bat                    # Cat replacement
     fd                     # Find replacement
     #gotop                  # Top replacement (system monitor)
     # i3lock-fancy           # Screen locker
     # ncdu                   # Fancy disk usage analyzer
     neofetch               # Fancy system information
     # GUI
     qutebrowser            # Web browser
     chromium               # Another web browser
     # zotero
     # numix-cursor-theme

     # Sound
     alsaTools
     alsaPlugins
     alsaUtils
     alsa-firmware
     pavucontrol
   ];

  environment.variables = {
    # QT_QPA_PLATFORM = "xcb";
    # QT_QPA_PLATFORMTHEME = "qt5ct";
    # QT_QUICK_CONTROLS_STYLE = "org.kde.desktop";
    # XCURSOR_THEME = "breeze_cursors";

    # Preferred applications
    EDITOR = "emacsclient -c";
    BROWSER = "qutebrowser";
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      # configFile = pkgs.writeText "default.pa" ''
      #   load-module module-bluetooth-policy
      #   load-module module-bluetooth-discover
      #   ## module fails to load with
      #   ##   module-bluez5-device.c: Failed to get device path from module arguments
      #   ##   module.c: Failed to load module "module-bluez5-device" (argument: ""): initialization failed.
      #   # load-module module-bluez5-device
      #   # load-module module-bluez5-discover
      # '';
    };
    bluetooth = {
      enable = true;
      extraConfig = "
        [General]
        Enable=Source,Sink,Media,Socket
      ";
    };
  };

  services = {
    # Enable emacs daemon, and set EDITOR to emacsclient
    emacs = {
      enable = true;
      defaultEditor = true;
    };

    fprintd.enable = true;

    # Power button invokes suspend, not shutdown.
    logind.extraConfig = "HandlePowerKey=suspend";

    # X
    xserver = {
      enable = true;
      # Enable touchpad support.
      libinput.enable = true;
      # Keyboard settings
      layout = "us";
      xkbVariant = "colemak";
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = true;
      desktopManager.gnome3.enable = true;
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
        extraGroups = [ "wheel" "networkmanager" "tty" "dialout" "input" ];
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
  system.stateVersion = "18.09"; # Did you read the comment?
}
