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
            (import ../overlays)
          ];
        };
      base-modules = [
        overlays
        ./configuration.nix
        home-manager.nixosModules.home-manager

      ];
      configured-home = extra-imports:
        {
        home-manager.users.matt = (import ../home/home.nix) {
          extra-imports = extra-imports;
        };
      };
    in
    {
    nixosConfigurations.oibri-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = base-modules ++ [
        ./oibri-nixos-hardware.nix
        ./t480s.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-t480s
        (configured-home [ ../home/personal.nix ])
      ];
    };
    nixosConfigurations.unity-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = base-modules ++ [
        ./unity-nixos-hardware.nix
        ./x1.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme
        (configured-home [ ../home/unity.nix ])
      ];
    };
  };
}
