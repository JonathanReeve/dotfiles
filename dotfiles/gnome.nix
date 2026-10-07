{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
     deja-dup               # Backups
     gthumb                 # Photos
     gnome-tweaks
     #gnomeExtensions.pop-shell
     gnomeExtensions.paperwm
     foliate                 # Ebooks
     polkit_gnome
     evince # PDFs and documents

     # GTK Themes
     arc-theme
     gnome-themes-extra
  ];
  services.gnome = {
      gnome-keyring.enable = true;
      gnome-online-accounts.enable = true;
      tinysparql.enable = true;
      localsearch.enable = true;
    };
  # Newer NixOS uses top-level `services.displayManager` and `services.desktopManager`
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
}
