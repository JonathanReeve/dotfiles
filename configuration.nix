# This is a configuration file for new servers to run Corpus-DB.

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/vda"; # Vultr
  };

  networking.hostName = "corpus-db-nixos"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    # Standard CLI tools
    wget vim fish git
    # Other
    libxml2 sqlite sqlite-interactive
    # Python
    pypi2nix
    (python36.withPackages(ps: with ps; [ jupyter virtualenvwrapper ]))
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  programs = {
    fish = {
      enable = true;
    };
  };

  security.pam.enableSSHAgentAuth = true;

  # Enable the OpenSSH daemon.
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
    };
    fail2ban = {
      enable = true;
      jails = {
        nophp = ''
          # Block an IP address if tries to access .php files
          # more than 5 times in 10 minutus.
          filter = nophp
          action = iptables-multiport[name=HTTP, port="http,https"]
          logpath = /var/log/wai/requests.log
          findtime = 600
          bantime = 600
          maxretry = 5
        '';
      };
    };
  };

  # This isn't working for some reason. It complains that
  # pkill isn't found in the PATH. 
  # systemd.services.corpus-db = {
  #   description = "Corpus DB Webserver";
  #   enable = true;
  #   serviceConfig = {
  #     Type = "forking";
  #     ExecStart = "/home/jon/corpus-db/result/bin/corpus-db";
  #     ExecStop = "${pkgs.pkill} corpus-db";
  #     Restart = "on-failure";
  #   };
  #   wantedBy = [ "default.target" ];
  # };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jon = {
    isNormalUser = true;
    uid = 1000;
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDqm3Uw8BKvlpTxvcPFUYt3uQ8V72odts2hqStl7CgZ8G4hXlQIc6m1BWaePq1beRCIHEj+h4Of5XiA/nsUk080ff6FwTM6i82P4TE59sbn4Qwtwu/+xNHUO6j3kfIRhR3amIsEeRdpDaX42YvVqVtquCNHQmcqeTSNqfwUKkZKP51tNqvGumPGbtcnQEYEeGOrOv0LOQ4YC83zjnOSYuWfwZ9QxI0FNi9QGG61BtZWmv2pML+AjuGKwaXQsGkFk2Z0JYCyQdYYeOq6jWrefdAAzbPUN9p8QSP5890tS7GgC9f8yQCspz7Ru92/9JO7pM9CthF/PLYIQHa7YIUvLNBN jon@jon-laptop" ];
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
  };

  users.users.eunseo = {
    isNormalUser = true;
    uid = 1001;
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7AahHEymAJLcWBdnHtioT/ue+U50wfXCjLRsGYg9gXyUF9/+x/M6NiJv6ZXu88FzaDxyW0ZYctWhi84d1lA4JwPxR6IozrjBseUJinLx6IugBumv0/nCDfZp8vmWzFzbFZXsZ/MNPhqLT/RTasQ/b9EycAQVvZD+hiMw6NGNx9Mfy6zdPRznOKiq+ig5uQ3wE+ymKmrb556iZt9T9ml25ZNYwcqWsvRCr45CjmrUCvUCT3Y45O4WC5C5FBnqZgHegXO+pTyIjHTk5k8AS5TvTDdrAzdkjswM+efG2eWbedM8xAJ/8H2CkZKqX5TgL7xijIaFD47zrdgaHQ6HXjySt eunseo@DN0a2236af.SUNet"
    ];
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
