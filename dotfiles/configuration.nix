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
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.plymouth.enable = true;
  networking.hostName = "jon-laptop"; # Define your hostname.
  networking.networkmanager.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    chromium.enablePepperFlash = true;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     nix-index
     fish       # Shell
     vim emacs  # Text editor
     pass       # Passwords
     aspell     # Spell checker
     home-manager
     networkmanager
     lsb-release
     gcc gnumake
     gnupg
     wget
     isync mu w3m # Mail
     pandoc # Document manipulation
     git    # Version control
     dropbox-cli
     # GUI
     qutebrowser    # Web browser
     chromium       # Another web browser
     zotero
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
     # KDE
     konversation # IRC
     gwenview     # Image viewer
     okular       # PDF viewer
     dolphin      # File manager
     kate         # Text editor
     ark          # Archive management
     spectacle    # Screenshots
     dragon       # Video player
   ];

  # HiDPI
  environment.variables.PLASMA_USE_QT_SCALING = "1";
  environment.variables.EDITOR = "emacsclient -c";
  environment.variables.BROWSER = "qutebrowser";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable emacs daemon, and set EDITOR to emacsclient
  services.emacs.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Keyboard
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl,colemak";

  # HiDPI
  services.xserver.dpi = 192;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.windowManager.bspwm.enable = true;

  # Shell
  programs.fish.enable = true;

  users.users.jon =
    { isNormalUser = true;
      home = "/home/jon";
      shell = pkgs.fish;
      description = "Jonathan Reeve";
      extraGroups = [ "wheel" "networkmanager" ];
    };
  users.users.systemrestore =
    { isNormalUser = true;
      home = "/home/systemrestore";
      shell = pkgs.fish;
      description = "System Restore";
      extraGroups = [ "wheel" "networkmanager" ];
    };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
