{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    pipestatus.url = "github:ftzm/pipestatus";
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager, pipestatus }:
    let
      overlays = { pkgs, ... }:
        {
          nixpkgs.overlays = [
            pipestatus.overlay
            (import ./overlays)
          ];
          nixpkgs.config.firefox.enableTridactylNative = true;
        };
      mkConfig = module-path:
        let
          args = import module-path { inherit nixos-hardware; };
          mkConfig' =
            { hardware # result of hardware scan
            , configuration # machine-specific nixos config
            , home # machine-specific home config
            , nixos-hardware-module ? null
            }:
            nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = builtins.filter (x: x != null) [
                overlays
                ./configuration/configuration.nix
                hardware
                nixos-hardware-module
                configuration
                home-manager.nixosModules.home-manager
                {
                  home-manager.users.ftzm = {
                    imports = [(import ./home/home.nix) home ];
                  };
                }
              ];
            };
        in mkConfig' args;
    in
    {
    nixosConfigurations.oibri-nixos = mkConfig ./machines/oibri-nixos;
    nixosConfigurations.leigheas = mkConfig ./machines/leigheas;
    nixosConfigurations.unity-nixos = mkConfig ./machines/unity-nixos;
  };
}
