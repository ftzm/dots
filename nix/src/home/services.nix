{
  services.udiskie.enable = true;

  # Keybase
  services.kbfs.enable = true;
  services.keybase.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };

}
