{
  description = "My NixOS configuration.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };
    nixos.url = "nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware"; 
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened"; # Retained for both laptops
    caelestia-shell.url = "github:caelestia-dots/shell";
    caelestia-cli.url = "github:caelestia-dots/cli";
    dms.url = "github:AvengeMedia/DankMaterialShell";
    dms-plugin-registry.url = "github:AvengeMedia/dms-plugin-registry";
  };
  outputs = inputs @ { self,
              nixos, 
              nixos-hardware,
              home-manager, 
              ...
            }:
    {
      # Define configurations for both laptops with new names
      nixosConfigurations.fw12 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ pkgs, ... }: {
            nixpkgs.overlays = [
              (final: prev: {
                # Patch dms-shell to fix click-outside and ensure toggle works correctly
                dms-shell-patched = (inputs.dms.packages.${pkgs.system}.default.override { }).overrideAttrs (old: {
                  postInstall = (old.postInstall or "") + ''
                    # 1. Wire the background click signal to the close function
                    substituteInPlace $out/share/quickshell/dms/Widgets/DankPopout.qml \
                      --replace-fail "signal backgroundClicked" "signal backgroundClicked; onBackgroundClicked: close()"
                    
                    # 2. Fix the contentWindow size to ensure it can catch clicks outside the widget
                    # We make it cover the screen when it should be visible
                    substituteInPlace $out/share/quickshell/dms/Widgets/DankPopout.qml \
                      --replace-fail "right: !useBackgroundWindow" "right: true" \
                      --replace-fail "bottom: _fullHeight || !useBackgroundWindow" "bottom: true"
                  '';
                });
              })
            ];
          })
          ./configuration.nix
          ./hardware-configuration-fw12.nix  # Import specific hardware config
          nixos-hardware.nixosModules.framework-12-13th-gen-intel
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.jon = { pkgs, ... }: {
              imports = [ ./home.nix
                          inputs.nix-doom-emacs-unstraightened.homeModule
                          inputs.caelestia-shell.homeManagerModules.default
                          inputs.dms.homeModules.dank-material-shell
                          inputs.dms-plugin-registry.modules.default
                          # inputs.dms.homeModules.niri
                        ];
              # Use our patched package
              programs.dank-material-shell.package = pkgs.dms-shell-patched;
              };
            }
        ];
      };

      nixosConfigurations.fw16 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          nixos-hardware.nixosModules.framework-16-7040-amd 
          ./hardware-configuration-fw16.nix  # Import specific hardware config
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.jon = { pkgs, ... }: {
              imports = [
                ./home.nix
                inputs.nix-doom-emacs-unstraightened.hmModule
              ];
            };
          }
        ];
      };
    };
}

