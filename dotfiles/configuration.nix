# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, lib, ... }:

{
  imports =
    [ ./gnome.nix
      ./python.nix
      #./R.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    # Enable magic sysrql (Alt+PrtSc) keys for recovery
    kernel.sysctl = { "kernel.sysrq" = 1; };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "btqca" "hci_qca" "hci_uart" "bluetooth" ];
    blacklistedKernelModules = [ "psmouse" ];
    tmp.cleanOnBoot = true;
    plymouth.enable = true;
    resumeDevice = "/dev/nvme0n1p3";
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "jon-laptop"; # Define your hostname .
    networkmanager.enable = true;
    firewall.checkReversePath = "loose";
  };

  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

  nixpkgs = {
    config = { allowUnfree = true;
               allowBroken = true;
    };
    overlays = [
    ];
  };

  console.useXkbConfig = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "eo.UTF-8";
    supportedLocales = [ "eo/UTF-8" "en_US.UTF-8/UTF-8" ];
  };

  # Fonts!
  fonts.packages = with pkgs; [
    fantasque-sans-mono
    font-awesome_5
    fira-code
    noto-fonts
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
    iosevka-comfy.comfy
  ];
  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
     # Nix stuff
     nix-index              # Indexing files for nix-locate
     nix-prefetch-git nix-prefetch-scripts # Help writing .nix files
     cabal2nix # pypi2nix
     cachix
     devenv

     # Security
     # yubico-pam yubioath-desktop yubikey-personalization
     # yubikey-manager # Provides ykman
     # yubikey-personalization-gui

     megasync             # Backups
     #megacmd
     keybase-gui          # Also backups
     #logseq               # Fancy notes

     # CLI
     fish                   # Shell
     vim                    # Text editors
     vale                   # Prose linting
     # aspell aspellDicts.en  # Spell checker
     (aspellWithDicts (dicts: with dicts; [ en en-computers en-science eo fr ]))
     pass encfs gopass      # Passwords and encryption
     light                  # Brightness control
     networkmanager
     tailscale
     gcc gnumake libtool
     gnupg
     wget
     isync w3m              # Mail
     protonmail-bridge
     protonmail-bridge-gui

     gnutls                 # For mail auth
     protonvpn-cli          # VPN
     pandoc
     zlib                   # For Pandoc development

     #direnv                # Essential project management thingy
     graphviz               # Simple charts
     xclip                  # Clipboard on the command line
     dict                   # Dictionary

     pywal
     wallust
     ranger

     # Building stuff
     cmake
     extra-cmake-modules

     stack
     (haskellPackages.ghcWithPackages (ps: with ps; [
       # pandoc-citeproc
       shake         # Build tool
       hlint         # Required for spacemacs haskell-mode
       hasktags      # Required for spacemacs haskell-mode
       hoogle        # Required for spacemacs haskell-mode
       lucid 
       turtle        # Scripting
       regex-compat
       #PyF
       HandsomeSoup
       tokenize
     ]))

     cabal-install
     texlive.combined.scheme-full
     typst
     git                    # Version control
     github-cli
     zip unzip                  # Archives

     texlive.combined.scheme-full
     git git-lfs            # Version control
     unzip                  # Archives
     file                   # File properties
     imagemagick            # Image manipulation
     libxml2
     sqlite sqlite-interactive # Sqlite

     # Elm
     elmPackages.elm
     elmPackages.elm-format
     # Julia
     julia-stable-bin
     # Scala
     dotty
     metals
     coursier
     # Minimal computing
     ranger highlight       # File manager
     scrot                  # Screenshots
     tree                   # Show file hierarchies
     mpv                    # Minimalist video player
     freetube
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
     chromium               # Another web browser
     firefox-wayland        # Yes, a third
     #nyxt                   # Why stop now?

     # Ugh
     zoom-us
     # calibre                # Ebooks
     libgourou                # Epub/PDF DRM

     ntfs3g ntfsprogs       # Windows drives compatibility

     # Sound
     alsa-tools
     #alsaPlugins
     alsa-utils
     alsa-firmware
     pavucontrol
     spotify

     # Keyboard stuff
     plover.dev
     waydroid

     # sway related
     swaybg
     wdisplays
     polkit
     wl-clipboard
     # autotiling

     # Hyprland
     hyprland
     mpvpaper

     # Web dev
     nodejs

     # Notetaking
     anytype

     # Editor
     vscode

     # Write books
     quarto
     #jupyter-book

     # AI
     aider-chat-full
   ];

  environment.variables = {
    # Preferred applications
    EDITOR = "emacsclient -c";
    BROWSER = "qutebrowser";
    CM_LAUNCHER = "rofi"; # Clipmenu
  };

  # Enable sound.
  hardware = {
    firmware = with pkgs; [ firmwareLinuxNonfree ];
    keyboard.qmk.enable = true;
    sensor.iio.enable = true;
    bluetooth.enable = true;
  };
  # home-manager.users.jon = import ./home.nix;
  # powerManagement = {
  #   enable = false;
  #   powertop.enable = false;
  # };

  services = {
    dictd = {
      enable = true;
      DBs = with pkgs.dictdDBs; [ wiktionary wordnet ];
    };
    desktopManager.cosmic.enable = true;
    flatpak.enable = true;
    fwupd.enable = true; # Firmware updates

    keybase.enable = true;
    kbfs = {
      enable = true;
      mountPoint = "%h/Keybase";
    };

    # localtime.enable = true;

    libinput = {
     enable = true;
     touchpad = {
       clickMethod = "clickfinger";
       disableWhileTyping = true;
     };
    };

    # Updated option name: see NixOS options for logind settings
    logind = {
      settings = {
        Login = {
          HandleLidSwitch = "suspend";
          HandlePowerKey = "suspend";
        };
      };
    };

    # Power management
    # upower.enable = true;

    # Plex media server
    plex = {
      user = "systemrestore";
      group = "users";
      enable = false;
      openFirewall = true;
    };

    # VPN
    tailscale.enable = true;

    # Security
    udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
    pcscd.enable = true;
    mozillavpn = {
      enable = true;
    };

    # X
    xserver = {
      enable = true;
      # Enable touchpad support.
      # Keyboard settings
      xkb = { 
        layout = "us";
        variant = "colemak";
      };
      desktopManager.session = [
        { name = "home-manager";
          start = ''${pkgs.stdenv.shell} $HOME/.xsession-hm 
	            & waitPID=$!''; 
        }
      ];
    };
  };

  # Shell
  programs = {
    hyprland.enable = true;
    chromium = {
      enable = true;
    };
    gnupg.agent = { enable = true; enableSSHSupport = true; };
    # hyprland.enable = true;
    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
    zsh.enable = true;
  };

  users.users = {
    jon =
      { isNormalUser = true;
        home = "/home/jon";
        shell = pkgs.zsh;
        description = "Jonathan Reeve";
        extraGroups = [ "audio" "wheel" "networkmanager" "tty" "dialout" "input" "docker" "video"];
      };
    systemrestore =
      { isNormalUser = true;
        home = "/home/systemrestore";
        shell = pkgs.zsh;
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

  # systemd.user.services.protonmail = {
  #   description = "Protonmail Bridge";
  #   enable = true;
  #   script =
  #     "${pkgs.protonmail-bridge}/bin/protonmail-bridge --log-level debug";
  #   path = [ pkgs.gnome.gnome-keyring ]; # HACK: https://github.com/ProtonMail/proton-bridge/issues/176
  #   wantedBy = [ "graphical-session.target" ];
  #   partOf = [ "graphical-session.target" ];
  # };
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "23.11"; # Did you read the comment?

  # virtualisation.anbox.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

}
