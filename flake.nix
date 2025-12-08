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
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-lutris.url = "github:NixOS/nixpkgs/1a7de5d740a244b99c53e6bff8c60b621637f687";
    nixpkgs-ftzmlab.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-iosevka.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    pipestatus.url = "github:ftzm/pipestatus";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix.url = "github:ryantm/agenix";
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = inputs @ {
    nixpkgs,
    nixpkgs-ftzmlab,
    home-manager,
    emacs-overlay,
    git-hooks,
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
    checks.${defaultSystem}.pre-commit-check = git-hooks.lib.${defaultSystem}.run {
      src = ./.;
      hooks = {
        deadnix.enable = true;
        alejandra.enable = true;
        #statix.enable = true;
        trufflehog.enable = true;
        nil.enable = true;
      };
    };

    devShells.${defaultSystem}.default = nixpkgs.legacyPackages.${defaultSystem}.mkShell {
      inherit (inputs.self.checks.${defaultSystem}.pre-commit-check) shellHook;
      buildInputs =
        inputs.self.checks.${defaultSystem}.pre-commit-check.enabledPackages
        ++ [
          nixpkgs.legacyPackages.${defaultSystem}.alejandra
        ];
    };

    formatter.${defaultSystem} = nixpkgs.legacyPackages.${defaultSystem}.alejandra;

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
      nuc = mkLabSystem {host = "nuc";};
      nas = mkLabSystem {host = "nas";};
      pi = mkLabSystem {
        host = "pi";
        system = "aarch64-linux";
      };
    };
    homeConfigurations.ftzm = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages."x86_64-linux".extend emacs-overlay.overlay;

      # Specify your home configuration modules here, for example,
      # the path to your home.nix.
      modules = [./machines/ftm-P14s/home.nix];

      # Optionally use extraSpecialArgs
      # to pass through arguments to home.nix
      extraSpecialArgs = {inherit inputs;};
    };
  };
}
