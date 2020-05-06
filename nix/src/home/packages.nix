{ config, pkgs, ... }:

let
  iosevkaLig = pkgs.callPackage ./iosevka.nix { };
  myEmacs = pkgs.emacs.override { inherit (pkgs) imagemagick; };
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  ps = (import (builtins.toPath "${config.home.homeDirectory}/dev/pipestatus/release.nix")).pipestatus;
in {
  home.packages = with pkgs; [
    ps
    nix
    cacert # nothing else works without these
    zlib.dev # also needed for a lot
    zlib.out # also needed for a lot
    glibcLocales # for non-broken locales in non-nixos

    # system
    dmenu
    ranger
    htop
    arandr
    pass
    xdotool
    (dunst.override { dunstify = true; })
    libnotify # for notify-send in scripts

    restic # backups

    acpi
    killall
    unzip
    pciutils
    brightnessctl
    gotop

    # office/document/media
    #libreoffice
    gimp
    slack
    vlc
    mpv
    zathura
    keybase-gui

    # latex
    # texlive.combined.scheme-full
    # pandoc

    # webz
    chromium
    firefox
    qutebrowser
    wget
    openvpn
    deluge

    # appearance
    hsetroot
    lxappearance
    arc-theme
    xorg.xcursorthemes
    gnome3.adwaita-icon-theme
    gnome3.gnome-tweak-tool
    vanilla-dmz

    # programming
    (emacsWithPackages
      (epkgs: [ epkgs.telega epkgs.emacs-libvterm epkgs.emms ]))
    neovim
    git
    git-crypt
    ag
    stack
    gnumake
    jq
    ripgrep

    # for spelling in emacs
    hunspell
    hunspellDicts.en_US-large
    aspell
    aspellDicts.en

    # cloud
    kops
    kubernetes

    # mpd
    mpc_cli
    ncmpcpp

    # email
    mu
    isync

    # fonts
    iosevkaLig
    source-sans-pro
    source-code-pro
    fira-mono
    fira-code

    # For Haskell dev
    cabal-install
    cabal2nix
    nix-prefetch-git

    # system management
    niv

    get-auth
    dbeaver
    kitty
    xdg_utils

    # hw
    neofetch
    hwinfo
    inxi
    glxinfo
    xorg.xdpyinfo
    dmidecode

    dconf
    zoom-us
    keybase
  ];
  xdg = {
    enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "org.pwmt.zathura.desktop" ];
      };
    };
  };
}
