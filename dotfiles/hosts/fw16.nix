{ ... }:

{
  imports = [
    ../hardware-configuration-fw16.nix
  ];

  networking.hostName = "fw16";

  boot.resumeDevice = "/dev/disk/by-uuid/2acff672-4159-41fe-a7a6-059dbb7e9493";
}
