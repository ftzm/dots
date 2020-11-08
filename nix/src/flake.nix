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
        };
      base-modules = [
        overlays
        ./configuration/configuration.nix
        home-manager.nixosModules.home-manager
      ];
      configured-home = extra-imports:
        {
        home-manager.users.ftzm = (import ./home/home.nix) {
          extra-imports = extra-imports;
        };
      };
    in
    {
    nixosConfigurations.oibri-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = base-modules ++ [
        nixos-hardware.nixosModules.lenovo-thinkpad-t480s
        ./machines/oibri-nixos/hardware.nix
        ./machines/oibri-nixos/t480s.nix
        (configured-home [ ./machines/oibri-nixos/home.nix ])
      ];
    };
    nixosConfigurations.unity-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = base-modules ++ [
        nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme
        ./machines/unity-nixos/hardware.nix
        ./machines/unity-nixos/x1.nix
        (configured-home [ ./machines/unity-nixos/home.nix ])
      ];
    };
  };
}
