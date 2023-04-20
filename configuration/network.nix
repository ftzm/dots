{ config, pkgs, lib, ... }:
let
  hosts = {
    leigheas = {
      wg = {
        ip = "10.0.100.2";
        listenPort = 51820;
        publicKey = "eLpLj1/WiCEW8w00A+HLPMkUGTGRCrRb1znESP43q0I=";
        clientOnly = true;
      };
    };
    # oibri-nixos = { };
    nas = {
      wg = {
        ip = "10.0.100.3";
        listenPort = 51830;
        publicKey = "9742uVx1meQuUeJvlwq63I4Bc1eI5XpOjQUVUC7eP2c=";
        clientOnly = false;
      };
    };
    nuc = {
      wg = {
        ip = "10.0.100.4";
        subnet = "10.0.0.248/29";
        listenPort = 51840;
        publicKey = "hkBdJ/i5Aei5RXNcoYluvJcScIoDz+Na8iVhiHQv6TA=";
        clientOnly = false;
      };
    };
    saoiste = {
      wg = {
        ip = "10.0.100.6";
        listenPort = 51860;
        publicKey = "x90h2zR9keCjpS8WksehZnXNgFwGIhwCxzy+QdQqqRA=";
        clientOnly = false;
      };
    };
  };
  host = config.networking.hostName;
  this = hosts."${host}";
  others = lib.attrsets.filterAttrs (k: v: k != host) hosts;
in {
  age.secrets."wireguard-private-key-${host}".file =
    ../secrets/wireguard-private-key + "-${host}.age";
  networking = {
    wireguard.interfaces = {
      wg0 = {
        ips = [ "${this.wg.ip}/24" ] ++ (if this.wg ? "subnet" then [this.wg.subnet] else []);
        inherit (this.wg) listenPort;
        privateKeyFile =
          config.age.secrets."wireguard-private-key-${host}".path;
        peers = lib.attrsets.mapAttrsToList (k: v: {
          inherit (v.wg) publicKey;
          allowedIPs = [ v.wg.ip] ++ (if v.wg ? "subnet" then [v.wg.subnet] else []);
          endpoint = if v.wg.clientOnly then
            null
          else
            "${k}.ftzmlab.xyz:${toString v.wg.listenPort}";
        }) others;
      };
    };
    extraHosts = lib.strings.concatMapStrings (x: x + "\n")
      ((lib.attrsets.mapAttrsToList (k: v: "${v.wg.ip} wg-${k}") others) ++ ["127.0.0.1 www.localhost.com"]);
  };
}
