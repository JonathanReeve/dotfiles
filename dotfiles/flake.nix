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
    # dms.url = "github:AvengeMedia/DankMaterialShell";
    # Local copy for testing
    dms = {
      url = "git+file:///home/jon/Programaroj/DankMaterialShell?ref=eo-traduko";
    };
    dms-plugin-registry.url = "github:AvengeMedia/dms-plugin-registry";
    iio-hyprland.url = "github:JeanSchoeller/iio-hyprland";
    hyprland.url = "github:hyprwm/Hyprland";
    hyprgrass = {
      url = "github:horriblename/hyprgrass";
      inputs.hyprland.follows = "hyprland";
    };
    danksearch = {
      url = "github:AvengeMedia/danksearch";
      inputs.nixpkgs.follows = "nixos";
    };
    citar-src = {
      url = "path:/home/jon/Programaroj/citar";
      flake = false;
    };
    antigravity-nix = {
      url = "github:jacopone/antigravity-nix";
      inputs.nixpkgs.follows = "nixos";
    };
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
        specialArgs = { inherit inputs; };
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
            home-manager.extraSpecialArgs = { inherit inputs; };
            home-manager.users.jon = { pkgs, ... }: {
              imports = [ ./home.nix
                          inputs.nix-doom-emacs-unstraightened.homeModule
                          inputs.dms.homeModules.dank-material-shell
                          inputs.dms-plugin-registry.modules.default
                          inputs.dms-plugin-registry.homeModules.default
                          inputs.danksearch.homeModules.dsearch
                        ];
              # Use our patched package
              programs.dank-material-shell.package = pkgs.dms-shell-patched;
              };
            }
        ];
      };

      nixosConfigurations.fw16 = nixos.lib.nixosSystem {
        modules = [
          ./configuration.nix
          nixos-hardware.nixosModules.framework-16-7040-amd 
          ./hardware-configuration-fw16.nix  # Import specific hardware config
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.extraSpecialArgs = { inherit inputs; };
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

