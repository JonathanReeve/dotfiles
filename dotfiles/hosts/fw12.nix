{ pkgs, inputs, ... }:

{
  imports = [
    ../hardware-configuration-fw12.nix
  ];

  networking.hostName = "fw12";

  boot.resumeDevice = "/dev/nvme0n1p3";

  # Framework 12 is a 2-in-1 convertible with accelerometer/auto-rotation
  hardware.sensor.iio.enable = true;
  environment.systemPackages = [
    inputs.iio-hyprland.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];
  home-manager.users.jon.wayland.windowManager.hyprland.settings.exec-once = [
    "iio-hyprland"
  ];
}
