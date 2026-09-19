{
  nixConfig = {
    tarball-ttl = 2592000;
    extra-substituters = [
      #   "https://nix-community.cachix.org"
      #   "https://cache.iog.io"
      #   "https://cache.zw3rk.com"
      "https://nixos-raspberrypi.cachix.org"
      "https://claude-code.cachix.org"
      "https://pi.cachix.org"
    ];
    extra-trusted-public-keys = [
      #   "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      #   "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      #   "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
      "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
      "pi.cachix.org-1:lGeoGJaZ5ZDabuRzkcD5EBTNnDM4HJ1vqeOxlWk1Flk="
    ];
  };

  inputs = {
    # nixos-unstable, not nixpkgs-unstable. The difference is what gates the
    # channel. nixos-unstable advances only when the `tested` aggregate of the
    # nixos/trunk-combined jobset passes, which includes the NixOS VM and
    # installer tests -- a revision that reaches it has been booted and
    # exercised as a NixOS system. nixpkgs-unstable advances on nixpkgs/trunk,
    # which builds packages and runs no NixOS system tests; it is there for
    # Darwin, standalone home-manager and nix-env consumers, for whom those
    # tests are meaningless. We build NixOS hosts from this input, so the gate
    # that matches what we do with it is the former.
    #
    # The stall that surfaced this (nixpkgs-unstable frozen at a32edd7 from
    # 09-17, missing the tree-sitter-cuda and nodejs 26.9.0 fixes that
    # nixos-unstable already had prebuilt) was the symptom. Relative freshness
    # is not the argument and does not reliably favour either channel.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-lutris.url = "github:NixOS/nixpkgs/1a7de5d740a244b99c53e6bff8c60b621637f687";
    # nas and nuc build from this; everything else tracks unstable. A release
    # branch stops receiving commits once the next release's overlap month ends,
    # and `nix flake update` goes on resolving it successfully forever -- so an
    # EOL branch is indistinguishable from an up-to-date one in every signal we
    # have. nixos-25.11 died on 2026-06-30 and sat here unnoticed for 73 days,
    # which is 73 days of no security backports on the storage box and the k3s
    # node. When the next release ships, move this within the overlap month;
    # `lastModified` in flake.lock standing still is the only symptom.
    nixpkgs-ftzmlab.url = "github:NixOS/nixpkgs/nixos-26.05";
    # A real pin, not a branch: this input exists so the font is built once and
    # then served from cache forever, and 28e1ac9c ("pin nixpkgs for it") meant
    # it to be one. It named a branch instead, so `nix flake update` re-resolved
    # it every cycle for five years. nixpkgs builds iosevka with buildNpmPackage
    # wired to `nodejs_latest`, so any bump that moves node invalidates the font:
    # #241 spent 2h26m compiling nodejs 26.9.0 from source because that build
    # fails upstream (NixOS/nixpkgs#564449) and Hydra had nothing to substitute.
    # Bump this deliberately when you want a newer Iosevka; nothing else should.
    nixpkgs-iosevka.url = "github:NixOS/nixpkgs/c7def046b9a883d46974757852106483d741586f";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pipestatus.url = "github:ftzm/pipestatus";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix.url = "github:ryantm/agenix";
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl.url = "github:nix-community/nixGL";
    claude-code-nix = {
      url = "github:sadjow/claude-code-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kimi-code = {
      url = "github:MoonshotAI/kimi-code";
    };
    pi = {
      url = "github:lukasl-dev/pi.nix";
    };
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/main";
    comin = {
      url = "github:nlewo/comin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:microvm-nix/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
      pi = import ./machines/pi {inherit inputs;};
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
