{ pkgs, ... }:

{
  users.extraUsers.ftzm = {
    createHome = true;
    extraGroups = [ "wheel" "video" "audio" "disk" "networkmanager" "docker" ];
    group = "users";
    home = "/home/ftzm";
    isNormalUser = true;
    # shell = pkgs.zsh;
    uid = 1000;
  };
}
