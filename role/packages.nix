{ config, pkgs, ... }:

let
  dmenuFuzzyPatch = builtins.fetchurl {
    url =
      "https://tools.suckless.org/dmenu/patches/fuzzymatch/dmenu-fuzzymatch-4.9.diff";
    sha256 = "0yababzi655mhpgixzgbca2hjckj16ykzj626zy4i0sirmcyg8fr";
  };
  tablePlus = pkgs.stdenv.mkDerivation {
    name = "TablePlus";

    libldapSrc = pkgs.fetchurl {
      url =
        "http://ftp.de.debian.org/debian/pool/main/o/openldap/libldap-2.5-0_2.5.13+dfsg-5_amd64.deb";
      sha256 = "sha256-S2ww9lVBScWUYo2UXtxgA/DuqNDME0FjjA5xN12xR+0=";
    };

    libsaslSrc = pkgs.fetchurl {
      url =
        "http://archive.ubuntu.com/ubuntu/pool/main/c/cyrus-sasl2/libsasl2-2_2.1.27+dfsg-2_amd64.deb";
      sha256 = "3OguEgUoKpB8lvlzUItzB7imn6Td46+Sl+YCFXM/LTA=";
    };

    tableplusSrc = pkgs.fetchurl {
      url =
        "https://deb.tableplus.com/debian/21/pool/main/t/tableplus/tableplus_0.1.212_amd64.deb";
      sha256 = "sha256-4+Z2zjpetEBrbvKG11GrCwzcR3IBTO0VLcQyUixFmSA=";
    };

    unpackPhase = ''
      runHook preUnpack
      dpkg-deb -x $tableplusSrc tableplus
      dpkg-deb -x $libldapSrc libldap
      dpkg-deb -x $libsaslSrc libsasl
      runHook postUnpack
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/lib"
      mkdir -p "$out/bin"
      mkdir -p "$out/resource"
      # deps
      cp -R libldap/usr/lib/x86_64-linux-gnu/* "$out/lib/"
      cp -R libsasl/usr/lib/x86_64-linux-gnu/* "$out/lib/"
      # tableplus
      cp -R "tableplus/opt/tableplus/tableplus" "$out/bin/tableplus"
      cp -R "tableplus/opt/tableplus/resource/" "$out/resource"
      chmod -R g-w "$out"
      # # Desktop file
      # mkdir -p "$out/share/applications"
      #cp "$desktopItem}/share/applications/"* "$out/share/applications"
      runHook postInstall
    '';

    nativeBuildInputs = with pkgs; [
      autoPatchelfHook
      dpkg
      makeWrapper
      wrapGAppsHook
    ];

    buildInputs = with pkgs; [
      gtksourceview
      gtksourceview4
      json-glib
      libgee
      libkrb5
      libsecret
      openldap
      stdenv.cc.cc.lib
    ];

    meta = with pkgs.stdenv.lib; {
      description = "Tableplus";
      homepage = "https://tableplus.com/";
      platforms = [ "x86_64-linux" ];
    };
  };
in {
  environment.systemPackages = with pkgs; [
    ps
    cacert # nothing else works without these
    zlib.dev # also needed for a lot
    zlib.out # also needed for a lot
    glibcLocales # for non-broken locales in non-nixos

    nzbhydra2
    unrar

    nyxt
    scrot
    go
    cachix

    # (tmux.override ({ withUtempter = false;}))
    tmux

    # system
    binutils
    file
    (dmenu.override ({ patches = [ dmenuFuzzyPatch ]; }))
    ranger
    htop
    arandr
    pass
    xdotool
    wtype
    dunst
    libnotify # for notify-send in scripts
    entr # file watcher + command firer
    sshfs-fuse # sshfs
    playerctl
    restic # backups
    acpi
    killall
    unzip
    pciutils
    brightnessctl
    gotop
    gnupg
    wireguard-tools
    nixfmt
    pavucontrol
    mpd-mpris
    dig
    chromium

    # office/document/media
    libreoffice
    gimp
    inkscape
    slack
    vlc
    (wrapMpv mpv-unwrapped { scripts = [ mpvScripts.mpris ]; })
    keybase-gui
    nomacs
    gnome.cheese
    qutebrowser
    wget
    openvpn
    deluge
    btop

    # appearance
    hsetroot
    lxappearance
    arc-theme
    xorg.xcursorthemes
    gnome3.adwaita-icon-theme
    vanilla-dmz

    # programming
    neovim
    git
    git-crypt
    silver-searcher
    stack
    gnumake
    jq
    ripgrep
    s-tui
    stress
    linuxPackages.turbostat
    gcc

    # for spelling (particularly in emacs)
    hunspell
    hunspellDicts.en_US-large
    aspell
    aspellDicts.en

    # cloud
    # kops
    kubectl
    k9s

    # mpd
    mpc_cli
    ncmpcpp

    # email
    mu
    isync

    tdesktop

    # fonts
    source-sans-pro
    source-code-pro
    jetbrains-mono

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

    # #games
    lgogdownloader
    playonlinux
    wine

    river
    rivercarro

    # code
    sqlfluff
    tablePlus

  ];
  home-manager.users.ftzm = {
    xdg = {
      enable = true;
      mimeApps = {
        enable = true;
        defaultApplications = {
          "application/pdf" = [ "org.pwmt.zathura.desktop" ];
          "text/html" = "firefox.desktop";
          "x-scheme-handler/http" = "firefox.desktop";
          "x-scheme-handler/https" = "firefox.desktop";
          "x-scheme-handler/about" = "firefox.desktop";
          "x-scheme-handler/unknown" = "firefox.desktop";
        };
      };
    };
    programs.zathura = {
      enable = true;
      options = {
        font = "Iosevka Lig normal ${toString (config.personal.font_size + 1)}";
        default-bg = "#282828";
        statusbar-bg = "#282828";
        statusbar-fg = "#ebdbb2";
      };
    };
    # for some reason necessary for firefox
    # programs.neovim = {
    #   enable = true;
    #   plugins = [ pkgs.vimPlugins.nvim-treesitter.withAllGrammars ];
    # };
    programs.firefox = {
      enable = true;
      package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
        # forceWayland = true;
        extraPolicies = { LocalFileLinks = [ "http://localhost:8080" ]; };
      };
      profiles.main = {
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.startup.homepage" = "www.google.com";
        };
        userChrome = ''

          /* Hide tabs */

          #TabsToolbar {
          visibility: collapse !important;
          margin-bottom: 21px !important;
          }

          # /* Hide sidebar header */

          # #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
          #   visibility: collapse !important;
          # }
          # .sidebar-splitter { display: none;}

        '';
      };
    };
  };
}
