{ config, lib, ... }:
let
  hosts = {
    leigheas = {
      wg = {
        ip = "10.0.100.2";
        listenPort = 51820;
        publicKey = "eLpLj1/WiCEW8w00A+HLPMkUGTGRCrRb1znESP43q0I=";
      };
    };
    #oibri-nixos = { };
    nas = {
      wg = {
        ip = "10.0.100.3";
        listenPort = 51830;
        publicKey = "9742uVx1meQuUeJvlwq63I4Bc1eI5XpOjQUVUC7eP2c=";
      };
    };
    nuc = {
      wg = {
        ip = "10.0.100.4";
        listenPort = 51840;
        publicKey = "hkBdJ/i5Aei5RXNcoYluvJcScIoDz+Na8iVhiHQv6TA=";
      };
    };
  };
  host = config.networking.hostName;
  this = hosts."${host}";
  others = lib.attrsets.filterAttrs (k: v: k != host) hosts;
in {
  age.secrets."wireguard-private-key-${host}".file =
    ./secrets/wireguard-private-key + "-${host}.age";
  networking = {
    wireguard.interfaces = {
      wg0 = {
        ips = [ "${this.wg.ip}/24" ];
        inherit (this.wg) listenPort;
        privateKeyFile =
          config.age.secrets."wireguard-private-key-${host}".path;
        peers = lib.attrsets.mapAttrsToList (k: v: {
          inherit (v.wg) publicKey;
          allowedIPs = [ v.wg.ip ];
          endpoint = "${k}.ftzmlab.xyz:${toString v.wg.listenPort}";
        }) others;
      };
    };
    extraHosts = lib.strings.concatMapStrings (x: x + "\n")
      (lib.attrsets.mapAttrsToList (k: v: "${v.wg.ip} wg-${k}") others);
  };
}
