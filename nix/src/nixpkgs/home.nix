{ config, pkgs, ... }:

let
  font_size = 10;
  isNixos = builtins.pathExists /etc/nixos;
  dots = "${config.home.homeDirectory}/.dots/";
  ps = import (pkgs.fetchFromGitHub {
    owner = "ftzm";
    repo = "pipestatus";
    rev = "fababa4417f394786c61eeccf13a2f371cb57a40";
    sha256 = "1ivb80y4b09d2s3zh63qw9wi3n5h36di735pp8rsnwz6hl6dhxk1";
  } + "/release.nix");

in {
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
    emacs
    neovim
    git
    git-crypt
    ag
    stack
    gnumake
    jq

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

    # custom
    ps.pipestatus
    ps.ps_scripts

    # fonts
    source-sans-pro
    fira-mono
    fira-code

    # For Haskell dev
    cabal-install
    cabal2nix
    nix-prefetch-git
  ];

  programs.zsh = {
    enable = true;
    plugins = [
      {
        name = "bashmarks";
        file = "bashmarks.sh";
        src = pkgs.fetchFromGitHub {
          owner = "huyng";
          repo = "bashmarks";
          rev = "342169ad48954ac24cb58ef479a6d3238e90f42f";
          sha256 = "1p7yd0vd8his8lf8jzn8xiv3cwiw0yswk94744jk7pdn5znfv5gp";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "650dd79d86f885f8802732e3748d8719e787d22f";
          sha256 = "11hynfzyv9ri7rjymj1ix016r8yqx2am692bjhq6qqdii6l8m40r";
        };
      }
    ];
    oh-my-zsh = {
      enable = true;
      plugins = [ "vi-mode" ];
      theme = "robbyrussell";
    };
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      ".." = "cd ..";
      ll = "ls -l";
      k = "kubectl";
    };
    history.ignoreDups = true;
    history.extended = true;
    initExtra = ''
      EDITOR='vim'
      export PATH=$HOME/bin:$HOME/.local/bin:$PATH
      # Ask for passwords in cli
      unset SSH_ASKPASS
      unset GIT_ASKPASS
      # unalias conflicting aliases for bashmarks
      unalias l
      [[ -f ~/.falcon ]] && source ~/.falcon
      # for home manager on ubuntu
      export NIX_PATH=$HOME/.nix-defexpr/channels''${NIX_PATH:+:}$NIX_PATH
    '';
  };

  services.udiskie.enable = true;
  services.syncthing.enable = true;

  # Keybase
  services.kbfs.enable = true;
  services.keybase.enable = true;

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "ag -g ''";
    defaultOptions = [
      "--color fg:223,hl:166,fg+:235,bg+:4,hl+:223"
      "--color info:246,prompt:223,spinner:142,pointer:166,marker:166 "
    ];
  };

  services.dunst = {
    enable = true;
    settings = {
      global = {
        geometry = "0x5-20+20";
        transparency = 0;
        notification_height = 0;
        separator_height = 3;
        padding = 12;
        horizontal_padding = 12;
        frame_width = 3;
        frame_color = "#bdae93";
        separator_color = "frame";
        idle_threshold = 120;
        font = "Fira Mono Medium ${toString font_size}";
        line_height = 0;
        markup = "full";
        format = ''
          <b>%s</b>
          %b'';
          alignment = "left";
          show_age_threshold = 60;
          word_wrap = "yes";
          ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = "true";
          hide_duplicate_count = false;
          show_indicators = "yes";
          icon_position = "off";
          sticky_history = "yes";
          history_length = 20;
      };
      urgency_low = {
        background = "#222222";
        foreground = "#888888";
        timeout = 10;
      };
      urgency_normal = {
        background = "#282828";
        foreground = "#ebdbb2";
        timeout = 10;
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = 0;
      };
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.urxvt = {
    enable = true;
    fonts = [
      "xft:Fira Mono Medium:Regular:size=10"
      "xft:Source Code Pro:Regular:size=10"
      "xft:DejaVu Sans Mono:Regular:size=10"
    ];
    extraConfig = {
      scrollBar = false;
      letterSpace = -1.6;
      smoothResize = true;
      interalBorder = 5;
      urgentOnBell = true;
      boldFont = null;
    };
  };
  xresources.properties = {
    "*background" = "#282828";
    "*foreground" = "#ebdbb2";
    "*color0" = "#282828";
    "*color8" = "#928374";
    "*color1" = "#cc241d";
    "*color9" = "#fb4934";
    "*color2" = "#98971a";
    "*color10" = "#b8bb26";
    "*color3" = "#d79921";
    "*color11" = "#fabd2f";
    "*color4" = "#458588";
    "*color12" = "#83a598";
    "*color5" = "#b16286";
    "*color13" = "#d3869b";
    "*color6" = "#689d6a";
    "*color14" = "#8ec07c";
    "*color7" = "#a89984";
    "*color15" = "#ebdbb2";
  } // (if isNixos then { } else { "Xft.dpi" = "180"; });

  fonts.fontconfig.enable = true;

  qt.enable = true;
  gtk.enable = true;

  programs.git = {
    enable = true;
    ignores = [ "*~" "#*#" ".mypy_cache" ".direnv" ];
    userEmail = "matthew@fitzsimmons.io";
    userName = "ftzm";
    extraConfig = {
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
    };
  };

  xsession = {
    enable = true;
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    initExtra = ''
      export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
      ${pkgs.hsetroot}/bin/hsetroot -solid "#282828" &
      rm /tmp/statuspipe.fifo; pipestatus &
      export FONT_SIZE=${toString font_size}
      # For non-broken locale on non-nixos
      xsetroot -cursor_name left_ptr
      # Mainly for ubuntu
      export XCURSOR_PATH=$HOME/.nix-profile/share/icons:$XCURSOR_PATH
    '';
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    } // (if isNixos then { } else { size = 64; });
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
