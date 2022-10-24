# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      #./cachix.nix
      ./gnome.nix
      ./hardware-configuration.nix
      ./python.nix
      ./R.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    # Enable magic sysrql (Alt+PrtSc) keys for recovery
    kernel.sysctl = { "kernel.sysrq" = 1; };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "btqca" "hci_qca" "hci_uart" "bluetooth" ];
    blacklistedKernelModules = [ "psmouse" ];
    cleanTmpDir = true;
    plymouth.enable = true;
    resumeDevice = "/dev/nvme0n1p4";
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "jon-laptop"; # Define your hostname .
    networkmanager.enable = true;
    useDHCP = false;
    # interfaces.wlp0s20f3.useDHCP = true;
    interfaces.enp0s13f0u1u3u1.useDHCP = true;
    # firewall.allowedTCPPorts = [ 8000 ]; # For local agenda server
  };

  nix = {
    # package = pkgs.nixFlakes; # For flakes
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

  # console.useXkbConfig = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "eo.UTF-8";
    supportedLocales = [ "eo/UTF-8" "en_US.UTF-8/UTF-8" ];
  };

  # Fonts!
  fonts.fonts = with pkgs; [
    fantasque-sans-mono
    font-awesome_5
    fira-code
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    font-awesome
    libertine
    victor-mono
    emacs-all-the-icons-fonts
    # font-fonts
    # monoid
    kochi-substitute # Japanese
  ];
  # Set your time zone.
  time.timeZone = "America/New_York";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
     # Nix stuff
     nix-index              # Indexing files for nix-locate
     nix-prefetch-git nix-prefetch-scripts # Help writing .nix files
     cabal2nix # pypi2nix
     nixfmt

     # Security
     # yubico-pam yubioath-desktop yubikey-personalization
     # yubikey-manager # Provides ykman
     # yubikey-personalization-gui

     megasync             # Backups
     megacmd
     keybase-gui          # Also backups
     logseq               # Fancy notes

     # CLI
     fish                   # Shell
     vim                    # Text editors
     vale                   # Prose linting
     aspell aspellDicts.en  # Spell checker
     pass encfs             # Passwords and encryption
     light                  # Brightness control
     networkmanager
     gcc gnumake
     gnupg
     wget
     isync mu w3m           # Mail
     protonmail-bridge
     gnutls                 # For mail auth
     protonvpn-cli          # VPN
     pandoc
     #direnv                 # Essential project management thingy
     graphviz               # Simple charts
     xclip                  # Clipboard on the command line
     x11idle
     dict                   # Dictionary

     pywal
     ranger

     # Building stuff
     cmake
     extra-cmake-modules

     (emacs.pkgs.withPackages (epkgs: with emacsPackages; [
       pdf-tools
     ]))

     stack
     (haskellPackages.ghcWithPackages (ps: with ps; [
       # pandoc-citeproc
       shake         # Build tool
       hlint         # Required for spacemacs haskell-mode
       apply-refact  # Required for spacemacs haskell-mode
       hasktags      # Required for spacemacs haskell-mode
       hoogle        # Required for spacemacs haskell-mode
       lucid 
       # stylish-haskell # Required for spacemacs haskell-mode
       # ^ marked as broken
       turtle        # Scripting
       regex-compat
       #PyF
       HandsomeSoup
       tokenize
       # chatter
     ]))
     # ihaskell

     cabal-install
     texlive.combined.scheme-full
     git                    # Version control
     github-cli
     unzip                  # Archives

     texlive.combined.scheme-full
     git git-lfs            # Version control
     unzip                  # Archives
     file                   # File properties
     imagemagick            # Image manipulation
     libxml2
     sqlite sqlite-interactive # Sqlite

     # Elm
     elmPackages.elm
     # elmPackages.elm-review
     elmPackages.elm-format
     # Julia
     julia-stable-bin
     # Scala
     dotty
     ammonite
     # Minimal computing
     ranger highlight       # File manager
     scrot                  # Screenshots
     tree                   # Show file hierarchies
     mpv                    # Minimalist video player
     #termite                # Vim-like modal terminal
     feh                    # Display images
     libnotify              # Notifications
     fzf                    # Fuzzy file finder
     ripgrep                # Fast grep replacement
     bat                    # Cat replacement
     fd                     # Find replacement
     sd                     # Sed replacement
     bottom                 # Top replacement (system monitor)
     ncdu                   # Fancy disk usage analyzer
     neofetch               # Fancy system information
     # GUI
     #qutebrowser            # Web browser
     #chromium               # Another web browser
     firefox-wayland         # Yes, a third

     # Ugh
     zoom-us
     # calibre                # Ebooks

     ntfs3g ntfsprogs       # Windows drives compatibility

     # Sound
     alsa-tools
     #alsaPlugins
     alsa-utils
     alsa-firmware
     pavucontrol

     # Keyboard stuff
     plover.dev
     # xorg.libxcb
     # xorg.xcbutil
     # libsForQt5.qtstyleplugins
     waydroid

     # sway related
     swaybg
     wdisplays
     polkit
     wl-clipboard
     # autotiling


     # Web dev
     nodejs
   ];

  environment.variables = {
    # Preferred applications
    EDITOR = "emacsclient -c";
    BROWSER = "qutebrowser";
    CM_LAUNCHER = "rofi"; # Clipmenu
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    firmware = with pkgs; [ firmwareLinuxNonfree ]; 
    pulseaudio = {
      enable = true;
    };
    sensor.iio.enable = true;
    bluetooth.enable = true;
  };
  # home-manager.users.jon = import ./home.nix;
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services = {
    dictd = {
      enable = true;
      DBs = with pkgs.dictdDBs; [ wiktionary wordnet ];
    };
    flatpak.enable = true;

    keybase.enable = true;
    kbfs = {
      enable = true;
      mountPoint = "%h/Keybase";
    };

    # localtime.enable = true;

    # Power button invokes suspend, not shutdown.
    logind = {
      extraConfig = "HandlePowerKey=suspend";
      lidSwitch = "suspend";
    };

    # Power management
    upower.enable = true;

    # Plex media server
    # plex = {
    #   user = "systemrestore";
    #   group = "users";
    #   enable = true;
    #   openFirewall = true;
    # };
    # Security
    udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
    pcscd.enable = true;

    # X
    xserver = {
      enable = true;
      # Enable touchpad support.
      libinput = {
        enable = true;
        touchpad = {
          clickMethod = "clickfinger";
          disableWhileTyping = true;
        };
      };
      # Keyboard settings
      layout = "us";
      xkbVariant = "colemak";
      desktopManager.session = [
        { name = "home-manager";
          start = ''${pkgs.stdenv.shell} $HOME/.xsession-hm 
	            & waitPID=$!''; }
        # { name = "sway"; start = ''${pkgs.sway}/bin/sway''; }
        { name = "newm";
          start = ''${pkgs.stdenv.shell} /home/jon/Aplikaĵoj/result/bin/start-newm & waitPID=$!'';
        }
      ];
      # windowManager.exwm = {
      #   enable = true;
      #   enableDefaultConfig = true;
      # };
    };
  };

  # Shell
  programs = {
    fish.enable = true;
    chromium = {
      enable = true;
    };
    gnupg.agent = { enable = true; enableSSHSupport = true; };
    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
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


  # Don't ask for my password *quite* as often.
  security = {
    sudo.extraConfig = "Defaults timestamp_timeout=60";
    # pam.u2f = {
    #   enable = true;
    #   # control = "required";
    #   # cue = true;
    #   # interactive = true;
    # };
    };

  systemd.user.services.protonmail = {
    description = "Protonmail Bridge";
    enable = true;
    script =
      "${pkgs.protonmail-bridge}/bin/protonmail-bridge --log-level debug";
    path = [ pkgs.gnome.gnome-keyring ]; # HACK: https://github.com/ProtonMail/proton-bridge/issues/176
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
  };
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.03"; # Did you read the comment?

  # virtualisation.anbox.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

}
