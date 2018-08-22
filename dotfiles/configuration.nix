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
    chromium.enablePepperFlash = true;
  };

  # Select internationalisation properties.
  i18n = {
    # HiDPI Font
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
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
     nix-index       # Indexing files for nix-locate
     nix-prefetch-git nix-prefetch-scripts # Help writing .nix files
     cabal2nix       # Haskell packages to .nix expressions
     home-manager    # Dotfiles management
     # CLI
     fish            # Shell
     vim emacs       # Text editor
     pass            # Passwords
     aspell aspellDicts.en  # Spell checker
     light           # Brightness control
     networkmanager
     lsb-release
     gcc gnumake
     gnupg
     wget
     isync mu w3m   # Mail
     pandoc         # Document manipulation
     git            # Version control
     dropbox-cli
     # GUI
     qutebrowser    # Web browser
     chromium       # Another web browser
     zotero
     numix-cursor-theme
     # Haskell
     stack
     ghc
     libxml2
     # Python
     (python3.withPackages(ps: with ps; [
       pandas
       jupyter
     ]))
     # Minimal computing
     ranger         # File manager
     scrot          # Screenshots
     rofi           # Launcher
     zathura        # PDF Viewer
     polybar        # System monitor, etc.
     compton        # Compositor
     mpv            # Video player
     termite        # Terminal
     pywal          # Wallpapers
     bspwm sxhkd    # Window manager
     # KDE
     konversation # IRC
     gwenview     # Image viewer
     okular       # PDF viewer
     dolphin      # File manager
     kate         # Text editor
     ark          # Archive management
     spectacle    # Screenshots
     #dragon       # Video player
   ];

  environment.variables = {
    # HiDPI Settings
    PLASMA_USE_QT_SCALING = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_QPA_PLATFORM = "xcb";
    QT_QUICK_CONTROLS_STYLE = "org.kde.desktop";
    QT_SCREEN_SCALE_FACTORS = "eDP1=2;HDMI1=2;VIRTUAL1=2;";

    # Preferred applications
    EDITOR = "emacsclient -c";
    BROWSER = "qutebrowser";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable emacs daemon, and set EDITOR to emacsclient
  services.emacs.enable = true;

  # X
  services.xserver = {
    enable = true;
    dpi = 180;
    # Enable touchpad support.
    libinput.enable = true;
    # Keyboard settings
    layout = "us";
    xkbVariant = "altgr-intl,colemak";
    # Enable the KDE Desktop Environment.
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
    windowManager.bspwm.enable = true;
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
        extraGroups = [ "wheel" "networkmanager" ];
      };
    systemrestore =
      { isNormalUser = true;
        home = "/home/systemrestore";
        shell = pkgs.fish;
        description = "System Restore";
        extraGroups = [ "wheel" "networkmanager" ];
      };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
