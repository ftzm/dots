{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    ps
    cacert # nothing else works without these
    zlib.dev # also needed for a lot
    zlib.out # also needed for a lot
    glibcLocales # for non-broken locales in non-nixos

    nzbhydra2
    # unrar

    nyxt
    scrot
    go
    cachix
    awscli2
    pcmanfm

    # (tmux.override ({ withUtempter = false;}))
    tmux
    wezterm

    # system
    binutils
    file
    # (dmenu.override {patches = [dmenuFuzzyPatch];})
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
    # nixfmt
    alejandra
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
    # (wrapMpv mpv-unwrapped {scripts = [mpvScripts.mpris];})
    keybase-gui
    nomacs
    # gnome.cheese
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
    adwaita-icon-theme
    vanilla-dmz

    # programming
    neovim
    git
    git-crypt
    git-filter-repo
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
    mpc
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

    dbeaver-bin
    kitty
    xdg-utils

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

    # code
    sqlfluff
    # tablePlus

    nautilus

    waypaper
    waybar
    nodejs_20
    perf
    nil
  ];

  programs.thunar.enable = true;
  services.tumbler.enable = true;
  services.gvfs.enable = true;

  home-manager.users.ftzm = {
    xdg = {
      enable = true;
      mimeApps = {
        enable = true;
        defaultApplications = {
          "application/pdf" = ["org.pwmt.zathura.desktop"];
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
        font = "Iosevka Lig normal 12";
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
        extraPolicies = {
          LocalFileLinks = ["http://localhost:8080"];
        };
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
