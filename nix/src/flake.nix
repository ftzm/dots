{
  inputs = {
    deploy-rs.url = "github:serokell/deploy-rs";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    pipestatus.url = "github:ftzm/pipestatus";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-20.09";
    agenix.url = "github:ryantm/agenix";
  };

  outputs = inputs@{ self, nixpkgs, deploy-rs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      mkUserSystem = host-config:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [ ./configuration host-config ];
        };
      nuc = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [ ./machines/nuc/configuration.nix ];
      };
      nas = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [ ./machines/nas ];
      };
    in {
      nixosConfigurations = {
        oibri-nixos = mkUserSystem ./machines/oibri-nixos;
        leigheas = mkUserSystem ./machines/leigheas;
        inherit nuc;
        inherit nas;
      };
      deploy.nodes = {
        nuc = {
          hostname = "nuc";
          profiles.system = {
            sshOpts = [ "-X" ];
            sshUser = "admin";
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.nuc;
          };
        };
        nas = {
          hostname = "nas";
          profiles.system = {
            sshOpts = [ "-X" ];
            sshUser = "admin";
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.nas;
          };
        };
      };
    };
}
