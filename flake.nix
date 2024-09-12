{
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/d6b3ddd253c578a7ab98f8011e59990f21dc3932";
    nixpkgs-ftzmlab.url = "github:NixOS/nixpkgs/nixos-24.05";
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
      eibhlis = nixpkgs.lib.nixosSystem {
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
    f = import ./config.nix {
      self = inputs.self;
      lib = nixpkgs.lib;
    };
  };
}
