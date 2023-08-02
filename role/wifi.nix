{ config, pkgs, ... }:
let
  homeWifiSsid = "waifu";
  onHomeWifi = pkgs.writeShellScriptBin "onHomeWifi" ''
    ssid=$(nmcli -t -f name,device connection show --active \
    | grep wlp0s20f3 \
    | cut -d\: -f1)
    [ "$ssid" = "${homeWifiSsid}" ]
  '';
in
{
  environment.systemPackages = [onHomeWifi];
}
