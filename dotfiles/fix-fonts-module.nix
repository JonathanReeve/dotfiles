{ config, lib, ... }:

{
  # This completely disables the problematic Home Manager fonts module for Darwin
  disabledModules = [
    "targets/darwin/fonts.nix"
  ];

  # Add a replacement module that does nothing
  home-manager.sharedModules = [{
    options.targets.darwin.fonts = {
      enable = lib.mkEnableOption "font installation" // { default = false; };
    };
  }];
}
