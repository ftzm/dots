{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-ftzmlab.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-iosevka.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    pipestatus.url = "github:ftzm/pipestatus";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix.url = "github:ryantm/agenix";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-ftzmlab, ... }:
    let
      defaultSystem = "x86_64-linux";
      mkUserSystem = host-config:
        nixpkgs.lib.nixosSystem {
          system = defaultSystem;
          specialArgs = { inherit inputs; };
          modules = [ ./configuration host-config ];
        };
      mkLabSystem = { host, system ? defaultSystem }:
        nixpkgs-ftzmlab.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [ (./machines/. + "/${host}") ];
        };
    in {
      nixosConfigurations = {
        leigheas = nixpkgs.lib.nixosSystem {
          system = defaultSystem;
          specialArgs = { inherit inputs; };
          modules = [ ./machines/leigheas ];
        };
        saoiste = nixpkgs.lib.nixosSystem {
          system = defaultSystem;
          specialArgs = { inherit inputs; };
          modules = [ ./machines/saoiste ];
        };
        nuc = mkLabSystem { host = "nuc"; };
        nas = mkLabSystem { host = "nas"; };
        # pi = mkLabSystem {
        #   host = "pi";
        #   system = "aarch64-linux";
        # };
      };
    };
}
