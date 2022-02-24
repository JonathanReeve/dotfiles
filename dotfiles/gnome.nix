{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
     deja-dup               # Backups
     gthumb                 # Photos
     gnome3.gnome-tweaks
     # gnome3.gnome-boxes
     gnomeExtensions.appindicator
     gnomeExtensions.caffeine
     gnomeExtensions.dash-to-dock
     gnomeExtensions.gsconnect
     gnomeExtensions.pop-shell
     foliate                 # Ebooks

     # GTK Themes
     theme-vertex
     arc-theme
     theme-obsidian2
     plano-theme
     orchis-theme
     materia-theme
     equilux-theme
     numix-gtk-theme
     gnome.gnome-themes-extra
     arc-icon-theme
     tela-icon-theme
     faba-icon-theme
     vimix-icon-theme
     qogir-icon-theme
     flat-remix-icon-theme
  ];
  services.gnome = {
      gnome-keyring.enable = true;
      gnome-online-accounts.enable = true;
      gnome-online-miners.enable = true;
      tracker.enable = true;
      tracker-miners.enable = true;
    };
  xserver = {
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
  };
  programs = {
    gnome-terminal.enable = true;
    # gnome-documents.enable = true;
    kdeconnect = {
      enable = true;
      package = pkgs.gnomeExtensions.gsconnect;
    };
  };
}
