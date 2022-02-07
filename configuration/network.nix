{ config, pkgs, lib, ... }:
let
  hosts = {
    leigheas = {
      wg = {
        ip = "10.0.100.2";
        listenPort = 51820;
        publicKey = "eLpLj1/WiCEW8w00A+HLPMkUGTGRCrRb1znESP43q0I=";
        client = true;
      };
    };
    #oibri-nixos = { };
    nas = {
      wg = {
        ip = "10.0.100.3";
        listenPort = 51830;
        publicKey = "9742uVx1meQuUeJvlwq63I4Bc1eI5XpOjQUVUC7eP2c=";
        client = false;
      };
    };
    nuc = {
      wg = {
        ip = "10.0.100.4";
        listenPort = 51840;
        publicKey = "hkBdJ/i5Aei5RXNcoYluvJcScIoDz+Na8iVhiHQv6TA=";
        client = false;
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
        ips = [ "${this.wg.ip}/24" ];
        inherit (this.wg) listenPort;
        privateKeyFile =
          config.age.secrets."wireguard-private-key-${host}".path;
        peers = lib.attrsets.mapAttrsToList (k: v: {
          inherit (v.wg) publicKey;
          allowedIPs = [ v.wg.ip ];
          endpoint = if v.wg.client then
            null
          else
            "${k}.ftzmlab.xyz:${toString v.wg.listenPort}";
        }) others;
      };
    };
    extraHosts = lib.strings.concatMapStrings (x: x + "\n")
      (lib.attrsets.mapAttrsToList (k: v: "${v.wg.ip} wg-${k}") others);
  };
  # Cheeky hack to restart the wireguard service on wifi connection.
  # Easiest way to re-resolve hostnames on new network.
  networking.networkmanager.dispatcherScripts = [{
    source = pkgs.writeText "upHook" ''
      if [ $1 != "wg0" ]; then
          case "$2" in
              #up|vpn-up)
              up)
                logger -s " interface $1 up, restarting wireguard"
                ${pkgs.systemd}/bin/systemctl restart wireguard-wg0.service
              ;;
              down|vpn-down)
              ;;
              hostname|dhcp4-change|dhcp6-change)
              # Do nothing
              ;;
              *)
              echo "$0: called with unknown action \`$2'" 1>&2
              exit 1
              ;;
          esac
      fi
    '';
    type = "basic";
  }];
}
