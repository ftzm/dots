{
  nixConfig = {
    # extra-substituters = [
    #   "https://nix-community.cachix.org"
    #   "https://cache.iog.io"
    #   "https://cache.zw3rk.com"
    # ];
    # extra-trusted-public-keys = [
    #   "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    #   "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    #   "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    # ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/3a05eebede89661660945da1f151959900903b6a";
    nixpkgs-lutris.url = "github:NixOS/nixpkgs/1a7de5d740a244b99c53e6bff8c60b621637f687";
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
        modules = [./machines/eibhlis];
      };
      nuc = mkLabSystem {host = "nuc";};
      nas = mkLabSystem {host = "nas";};
      pi = mkLabSystem {
        host = "pi";
        system = "aarch64-linux";
      };
    };
    f = import ./config.nix {
      self = inputs.self;
      lib = nixpkgs.lib;
    };
  };
}
