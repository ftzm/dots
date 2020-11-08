{ pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };
  users.extraUsers.ftzm = {
    createHome = true;
    extraGroups = [ "wheel" "video" "audio" "disk" "networkmanager" "docker" ];
    group = "users";
    home = "/home/ftzm";
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };
}
