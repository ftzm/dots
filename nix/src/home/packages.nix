{ config, pkgs, ... }:

let
  iosevkaLig = pkgs.callPackage ./iosevka.nix { };
  myEmacs = pkgs.emacs.override { inherit (pkgs) imagemagick; };
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  ps = (import (builtins.toPath
    "${config.home.homeDirectory}/dev/pipestatus/release.nix")).pipestatus;
  emms-taglib = pkgs.stdenv.mkDerivation {
    name = "emms-taglib";
    src = pkgs.fetchurl {
      url = "ftp://ftp.gnu.org/gnu/emms/emms-5.4.tar.gz";
      sha256 = "1nd7sb6pva7qb1ki6w0zhd6zvqzd7742kaqi0f3v4as5jh09l6nr";
    };

    buildInputs = [ pkgs.taglib ];
    buildPhase = "make emms-print-metadata";
    installPhase = ''
      mkdir -p $out/bin
      cp src/emms-print-metadata $out/bin
    '';

    meta = with pkgs.lib; {
      description = "EMMS TagLib shim";
      homepage = "https://www.gnu.org/software/emms/";
      license = licenses.gpl3Plus;
      #   maintainers = with maintainers; [ your-name-here ];
      platforms = platforms.linux ++ platforms.darwin;
    };
  };
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
    entr # file watcher + command firer
    sshfsFuse # sshfs

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
    (wrapMpv mpv-unwrapped { scripts = [ mpvScripts.mpris ]; })
    zathura
    keybase-gui

    # latex
    # texlive.combined.scheme-full
    # pandoc

    # webz
    #firefox
    chromium
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
      (epkgs: [ epkgs.telega epkgs.emacs-libvterm epkgs.emms emms-taglib]))
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
    rofi-filer
    rofi-dmenu
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
    w3m

    steam

    # dev
    nodejs-12_x
    nodePackages.typescript
    #gcloud
    google-clasp
    kubernetes-helm

    tridactyl-native
    next

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
