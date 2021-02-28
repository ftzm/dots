{ config, pkgs, ... }:

let
  iosevkaLig = pkgs.callPackage ./iosevka.nix {};
  custom-emacs = pkgs.callPackage ./emacs.nix {};
in {
  home.packages = with pkgs; [
    ps
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
    playerctl
    restic # backups
    acpi
    killall
    unzip
    pciutils
    brightnessctl
    gotop
    gnupg
    wireguard
    mpd-mpris

    # office/document/media
    libreoffice
    gimp
    slack
    vlc
    (wrapMpv mpv-unwrapped { scripts = [ mpvScripts.mpris ]; })
    keybase-gui
    nomacs

    # webz
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
    custom-emacs
    neovim
    git
    git-crypt
    ag
    stack
    gnumake
    jq
    ripgrep

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

    # fonts
    iosevkaLig
    source-sans-pro
    source-code-pro

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
    #w3m

    lgogdownloader
    playonlinux
    wine

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
  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Lig normal ${toString (config.personal.font_size + 1)}";
      default-bg = "#282828";
      statusbar-bg = "#282828";
      statusbar-fg = "#ebdbb2";
    };
  };
  programs.firefox = {
    enable = true;
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

          /* Hide sidebar header */

          #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
            visibility: collapse !important;
          }
          .sidebar-splitter { display: none;}

          /* Autohide sidebar */

          :root {
            --sidebar-hover-width: 8px;
            --sidebar-visible-width: 320px;
          }
          #sidebar-box {
            position: relative !important;
            transition: all 200ms !important;
            min-width: var(--sidebar-hover-width) !important;
            max-width: var(--sidebar-hover-width) !important;
            opacity: 0 !important;
            transition: all 250ms cubic-bezier(0.075, 0.820, 0.165, 1.000);
          }
          #sidebar-box:hover {
            transition: all 200ms !important;
            min-width: var(--sidebar-visible-width) !important;
            max-width: var(--sidebar-visible-width) !important;
            margin-right: calc((var(--sidebar-visible-width) - var(--sidebar-hover-width)) * -1) !important;
            z-index:1;
            opacity: 1 !important;
            transition: all 250ms cubic-bezier(0.075, 0.820, 0.165, 1.000);
          }

          #sidebar {
            opacity: 0 !important;
          }

          #sidebar:hover {
            opacity: 1 !important;
          }


        '';
    };
  };
}
