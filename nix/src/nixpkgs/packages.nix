{ pkgs, ... }:

let
  iosevkaLig = pkgs.callPackage ./iosevka.nix {};
  myEmacs = pkgs.emacs.override { inherit (pkgs) imagemagick; };
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in

{
  home.packages = with pkgs; [
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
    libreoffice
    gimp
    slack
    vlc
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

    # fun
    # steam define in system
    discord

    # appearance
    hsetroot
    lxappearance
    arc-theme
    xorg.xcursorthemes
    gnome3.adwaita-icon-theme
    gnome3.gnome-tweak-tool
    vanilla-dmz

    # programming
    (emacsWithPackages (epkgs: [epkgs.telega]))
    neovim
    git
    git-crypt
    ag
    stack
    gnumake
    jq

    # for spelling in emacs
    hunspell
    hunspellDicts.en_US-large

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

    # fonts
    iosevkaLig
    source-sans-pro
    fira-mono
    fira-code

    # For Haskell dev
    cabal-install
    cabal2nix
    nix-prefetch-git

    # system management
    niv

    pkgs.get-auth

  ];
}
