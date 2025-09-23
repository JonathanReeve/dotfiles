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

     # GTK Themes
     theme-vertex
     arc-theme
     theme-obsidian2
     plano-theme
     orchis-theme
     materia-theme
     equilux-theme
     numix-gtk-theme
     gnome-themes-extra
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
      tinysparql.enable = true;
      localsearch.enable = true;
    };
  # Newer NixOS uses top-level `services.displayManager` and `services.desktopManager`
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
}
