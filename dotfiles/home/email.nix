{ pkgs, vars, ... }:

let
  commonMbsync = patterns: {
    enable = true;
    create = "maildir";
    expunge = "both";
    inherit patterns;
    extraConfig.channel = {
      MaxMessages = 2000;
      ExpireUnread = "yes";
    };
  };
in
{
  accounts.email = {
    maildirBasePath = vars.maildir;
    accounts = {
      gmail = {
        address = "jon.reeve@gmail.com";
        userName = "jon.reeve@gmail.com";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass gmail";
        primary = true;
        mu.enable = true;
        mbsync = commonMbsync [ "INBOX" "Lists" "[Gmail]/Sent Mail" ];
        realName = vars.name;
        thunderbird.enable = true;
      };
      columbia = {
        address = "jonathan.reeve@columbia.edu";
        userName = "jpr2152@columbia.edu";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass lionmail";
        mu.enable = true;
        mbsync = commonMbsync [ "INBOX" "Homework" "Lists" "[Gmail]/Sent Mail" ];
        realName = vars.name;
        thunderbird.enable = true;
      };
      protonmail = {
        address = "jonathan@jonreeve.com";
        userName = "jonathan@jonreeve.com";
        passwordCommand = "${pkgs.pass}/bin/pass show 127.0.0.1:1143/jonathan@jonreeve.com";
        imap = {
          host = "127.0.0.1";
          port = 1143;
          tls = {
            enable = true;
            useStartTls = true;
            certificatesFile = "/home/jon/.config/protonmail/bridge/cert.pem";
          };
        };
        smtp = {
          host = "127.0.0.1";
          port = 1025;
          tls = {
            enable = true;
            useStartTls = true;
            certificatesFile = "/home/jon/.config/protonmail/bridge/cert.pem";
          };
        };
        mu.enable = true;
        mbsync = commonMbsync [ "INBOX" "Labels/Lists" "Sent" "Drafts" "Spam" ];
        realName = vars.name;
        thunderbird.enable = true;
      };
    };
  };

  programs = {
    mbsync.enable = true;
    mu.enable = true;
    thunderbird = {
      enable = true;
      profiles.default.isDefault = true;
    };
  };
}
