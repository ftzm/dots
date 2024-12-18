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
    nixpkgs.url = "github:NixOS/nixpkgs/2d2a9ddbe3f2c00747398f3dc9b05f7f2ebb0f53";
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
    home-manager,
    emacs-overlay,
    ...
  }: let
    defaultSystem = "x86_64-linux";
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
      # pi = mkLabSystem {
      #   host = "pi";
      #   system = "aarch64-linux";
      # };
    };
    homeConfigurations.ftzm = home-manager.lib.homeManagerConfiguration {
      pkgs = (nixpkgs.legacyPackages."x86_64-linux").extend emacs-overlay.overlay;

      # Specify your home configuration modules here, for example,
      # the path to your home.nix.
      modules = [ ./machines/ftm-P14s/home.nix ];

      # Optionally use extraSpecialArgs
      # to pass through arguments to home.nix
        extraSpecialArgs = {inherit inputs;};
    };
  };
}
