{pkgs, ...}: {
  home-manager.users.ftzm = {
    services.udiskie.enable = true;

    # Keybase
    # services.kbfs.enable = true;
    # services.keybase.enable = true;

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry-gnome3;
    };
  };
  services.dbus.packages = [pkgs.gcr];
}
