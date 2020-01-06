# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelParams = [ "pci=nomsi" ];
    # The below doesn't work, and causes this computer not to wake up from suspend
    # kernelParams = [ "mem_sleep_default=deep" ]; #TODO: break out into C930 module
    kernelPackages = pkgs.linuxPackages_5_3;
    cleanTmpDir = true;
    plymouth.enable = true;
    resumeDevice = "/dev/nvme0n1p7";
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "jon-laptop"; # Define your hostname.
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.wlp107s0.useDHCP = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  console = {
    useXkbConfig = true;
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "eo.UTF-8";
    #supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  # Fonts!
  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    font-awesome-ttf
    libertine
    victor-mono
    emacs-all-the-icons-fonts
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
     fish # xonsh             # Shell
     vim emacs              # Text editors
     vale                   # Prose linting
     aspell aspellDicts.en  # Spell checker
     pass encfs             # Passwords and encryption
     light                  # Brightness control
     networkmanager
     gcc gnumake
     gnupg
     wget
     isync mu w3m           # Mail
     pandoc

     (haskellPackages.ghcWithPackages (ps: with ps; [
       pandoc-citeproc
       shake         # Build tool
       hlint         # Required for spacemacs haskell-mode
       apply-refact  # Required for spacemacs haskell-mode
       hasktags      # Required for spacemacs haskell-mode
       hoogle        # Required for spacemacs haskell-mode
       stylish-haskell # Required for spacemacs haskell-mode
       turtle
       regex-compat
     ]))
     # haskellPackages.pandoc-crossref
     # haskellPackages.stylish-haskell
     # haskellPackages.hakyll
     # iio-sensor-proxy       # Accelerometer, gyroscope, etc.  #TODO: break out into C930 module
     texlive.combined.scheme-full
     git git-lfs            # Version control
     unzip                  # Archives
     file                   # File properties
     imagemagick            # Image manipulation
     libxml2
     sqlite sqlite-interactive # Sqlite
     # Python Development
     pipenv
     (python3.withPackages(ps: with ps; [
       pandas
       matplotlib
       #scikitlearn
       altair
       #vega
       jupyter
       jupyterlab
       virtualenvwrapper
       nltk
       pip
       # numpy
       # scikitlearn
       # textblob
       # word2vec
       # spacy
     ]))
     # Elm
     # elmPackages.elm
     # Minimal computing
     ranger highlight       # File manager
     scrot                  # Screenshots
     tree                   # Show file hierarchies
     # lftp                   # Fast file transfers
     compton
     autojump               # Jump around! With `j`
     mpv                    # Minimalist video player
     termite                # Vim-like modal terminal
     # pywal                  # Wallpapers
     feh                    # Display imaes
     dunst libnotify        # Notifications
     weechat                # IRC
     fzf                    # Fuzzy file finder
     ag                     # Fast grep replacement
     ripgrep                # Another fast grep replacement
     bat                    # Cat replacement
     fd                     # Find replacement
     gotop                  # Top replacement (system monitor)
     # i3lock-fancy           # Screen locker
     ncdu                   # Fancy disk usage analyzer
     neofetch               # Fancy system information
     # GUI
     qutebrowser            # Web browser
     chromium               # Another web browser
     firefox                # Yes, a third

     # Gnome
     # deja-dup               # Backups 

     ntfs3g ntfsprogs       # Windows drives compatibility

     # Sound
     #alsaTools
     #alsaPlugins
     #alsaUtils
     alsa-firmware
     pavucontrol

     # KDE
     okular
     dolphin
     kate
     dragon
     # kdeApplications.kmail
     # kdeApplications.kmail-account-wizard
     # kdeApplications.kmailtransport
     # kmail
     # accounts-qt
     # kontact
     gwenview
     ark
     thunderbird

   ];

  environment.variables = {
    # Preferred applications
    EDITOR = "emacsclient -c";
    BROWSER = "qutebrowser";
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudio;
      # package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };
    sensor.iio.enable = true;
    bluetooth.enable = true;
  };

  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services = {
    # Enable emacs daemon, and set EDITOR to emacsclient
    #emacs = {
    #  enable = true;
    #  defaultEditor = true;
    #};

    # gnome3 = {
    #   gnome-keyring.enable = true;
    #   gnome-online-accounts.enable = true;
    #   gnome-online-miners.enable = true;
    #   tracker.enable = true;
    #   tracker-miners.enable = true;
    # };

    flatpak.enable = true;

    localtime.enable = true;

    # Power button invokes suspend, not shutdown.
    logind = {
      extraConfig = "HandlePowerKey=suspend";
      lidSwitch = "suspend";
    };

    # Power management
    upower.enable = true;

    # X
    xserver = {
      enable = true;
      # Enable touchpad support.
      libinput = {
        enable = true;
        clickMethod = "clickfinger";
      };
      # Keyboard settings
      layout = "us";
      xkbVariant = "colemak";
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
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
    gnome-documents.enable = true;
    xonsh.enable = true;
    light.enable = true;
    gnupg.agent = { enable = true; enableSSHSupport = true; };
  };

  users.users = {
    jon =
      { isNormalUser = true;
        home = "/home/jon";
        shell = pkgs.fish;
        description = "Jonathan Reeve";
        extraGroups = [ "audio" "wheel" "networkmanager" "tty" "dialout" "input" "docker" "video"];
      };
    systemrestore =
      { isNormalUser = true;
        home = "/home/systemrestore";
        shell = pkgs.fish;
        description = "System Restore";
        extraGroups = [ "wheel" "networkmanager" ];
      };
  };

  # virtualisation.anbox.enable = true;

  # Don't ask for my password *quite* as often.
  security.sudo.extraConfig = "Defaults timestamp_timeout=60";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  virtualisation.docker.enable = true;

}
