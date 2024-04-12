{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/d6b3ddd253c578a7ab98f8011e59990f21dc3932";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    #nixpkgs.url = "github:NixOS/nixpkgs/db1ee6dfe67b724b78e995fae47f6aad56ecc9b2";
    #nixpkgs.url = "path:/home/ftzm/dev/nixpkgs";
    nixpkgs-ftzmlab.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-iosevka.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    pipestatus.url = "github:ftzm/pipestatus";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix.url = "github:ryantm/agenix";
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
  };

  outputs = inputs @ {
    nixpkgs,
    nixpkgs-ftzmlab,
    ...
  }: let
    defaultSystem = "x86_64-linux";
    mkUserSystem = host-config:
      nixpkgs.lib.nixosSystem {
        system = defaultSystem;
        specialArgs = {inherit inputs;};
        modules = [./configuration host-config];
      };
    mkLabSystem = {
      host,
      system ? defaultSystem,
    }:
      nixpkgs-ftzmlab.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs;};
        modules = [(./machines/. + "/${host}")];
      };
  in {
    nixosConfigurations = {
      leigheas = nixpkgs.lib.nixosSystem {
        system = defaultSystem;
        specialArgs = {inherit inputs;};
        modules = [./machines/leigheas];
      };
      saoiste = nixpkgs.lib.nixosSystem {
        system = defaultSystem;
        specialArgs = {inherit inputs;};
        modules = [./machines/saoiste];
      };
      eachtrai = nixpkgs.lib.nixosSystem {
        system = defaultSystem;
        specialArgs = {inherit inputs;};
        modules = [./machines/eachtrai];
      };
      nuc = mkLabSystem {host = "nuc";};
      nas = mkLabSystem {host = "nas";};
      # pi = mkLabSystem {
      #   host = "pi";
      #   system = "aarch64-linux";
      # };
    };
  };
}
