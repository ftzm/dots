{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-ftzmlab.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-iosevka.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-20.09";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    deploy-rs.url = "github:serokell/deploy-rs";
    pipestatus.url = "github:ftzm/pipestatus";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix.url = "github:ryantm/agenix";
  };

  outputs = inputs@{ self, nixpkgs, deploy-rs, nixpkgs-ftzmlab, ... }:
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
      mkDeployNode = { host, networkHost ? host }: {
        hostname = "${networkHost}";
        profiles.system = {
          sshUser = "admin";
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos
            self.nixosConfigurations."${host}";
          autoRollback = false;
          magicRollback = false;
        };
      };
    in {
      nixosConfigurations = {
        oibri-nixos = mkUserSystem ./machines/oibri-nixos;
        leigheas = mkUserSystem ./machines/leigheas;
        nuc = mkLabSystem { host = "nuc"; };
        nas = mkLabSystem { host = "nas"; };
        pi = mkLabSystem {
          host = "pi";
          system = "aarch64-linux";
        };
      };
      deploy.nodes = {
        nuc = mkDeployNode {
          host = "nuc";
          networkHost = "wg-nuc";
        };
        nas = mkDeployNode {
          host = "nas";
          networkHost = "wg-nas";
        };
        pi = mkDeployNode { host = "pi"; };
      };
    };
}
