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
    in
    {
    nixosConfigurations.unity-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        overlays
        ./unity-nixos-hardware.nix
        ./unity-nixos-state-version.nix
        ./x1.nix
        ./configuration.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.matt = (import ../home/home.nix) { model = "ThinkPad X1 Extreme 2nd"; };
        }
      ];
    };
  };
}
