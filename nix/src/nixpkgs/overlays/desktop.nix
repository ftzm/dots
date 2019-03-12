self: super: {
  desktop = self.buildEnv {
    name = "desktop";
    paths = with self.pkgs; [
      nix cacert # nothing else works without these

      # system
      dmenu
      rxvt_unicode
      zathura
      ranger
      htop
      arandr
      termite
      pass
      xdotool
      vlc
      slack
      firefox
      (dunst.override { dunstify = true;})
      libnotify # for notify-send in scripts
      acpi
      jq
      killall

      # webz
      chromium
      firefox
      qutebrowser
      wget
      openvpn

      # fun
      steam

      # appearance
      hsetroot
      lxappearance
      arc-theme
      xorg.xcursorthemes
      gnome3.adwaita-icon-theme
      gnome3.gnome-tweak-tool
      vanilla-dmz

      # programming
      emacs
      neovim
      git
      git-crypt
      direnv
      fzf
      ag
      stack
      gnumake

      # cloud
      kops
      kubernetes

      # mpd
      mpc_cli
      ncmpcpp

      # email
      mu
      isync
      protonmail-bridge
    ];
  };
}
