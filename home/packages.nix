{ config, pkgs, ... }:

let
  custom-emacs = pkgs.callPackage ./emacs.nix { };
  homeWifiSsid = "waifu";
  onHomeWifi =  pkgs.writeShellScriptBin "onHomeWifi" ''
    ssid=$(nmcli -t -f name,device connection show --active \
             | grep wlp0s20f3 \
             | cut -d\: -f1)
    [ "$ssid" = "${homeWifiSsid}" ]
  '';
  dmenuFuzzyPatch = builtins.fetchurl {
    url = "https://tools.suckless.org/dmenu/patches/fuzzymatch/dmenu-fuzzymatch-4.9.diff";
    sha256 = "0yababzi655mhpgixzgbca2hjckj16ykzj626zy4i0sirmcyg8fr";
  };
in {
  home.packages = with pkgs; [
    ps
    cacert # nothing else works without these
    zlib.dev # also needed for a lot
    zlib.out # also needed for a lot
    glibcLocales # for non-broken locales in non-nixos

    nyxt
    scrot
    go
    cachix

    # system
    binutils
    file
    dmenu
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
    bpytop

    # appearance
    hsetroot
    lxappearance
    arc-theme
    xorg.xcursorthemes
    gnome3.adwaita-icon-theme
    vanilla-dmz

    # programming
    custom-emacs
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

    # for spelling (particularly in emacs)
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
  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Lig normal ${toString (config.personal.font_size + 1)}";
      default-bg = "#282828";
      statusbar-bg = "#282828";
      statusbar-fg = "#ebdbb2";
    };
  };
  home.stateVersion = "21.05";
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
      forceWayland = true;
      extraPolicies = {
        LocalFileLinks = ["http://localhost:8080"];
      };
    };
    profiles.main = {
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "browser.startup.homepage" = "www.google.com";
      };
      # userChrome = ''

      #     /* Hide tabs */

      #     #TabsToolbar {
      #       visibility: collapse !important;
      #       margin-bottom: 21px !important;
      #     }

      #     /* Hide sidebar header */

      #     #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
      #       visibility: collapse !important;
      #     }
      #     .sidebar-splitter { display: none;}

      #   '';
    };
  };
}
