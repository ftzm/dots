{ config, pkgs, ... }:

let dots = "${config.home.homeDirectory}/.dots/"; in

{

  home.packages = with pkgs; [
      nix cacert # nothing else works without these
      zlib.dev # also needed for a lot
      zlib.out# also needed for a lot

      # system
      dmenu
      rxvt_unicode
      ranger
      htop
      arandr
      termite
      pass
      xdotool
      (dunst.override { dunstify = true;})
      libnotify # for notify-send in scripts
      acpi
      killall
      unzip
      pciutils

      # office/document/media
      libreoffice
      gimp
      slack
      vlc
      zathura

      # latex
      texlive.combined.scheme-full
      pandoc

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
    initExtra = "
    EDITOR='vim'
    export PATH=\"$PATH:/home/matt/.local/bin\"
    export PATH=\"$PATH:/home/matt/bin\"
    # Ask for passwords in cli
    unset SSH_ASKPASS
    unset GIT_ASKPASS
    # unalias conflicting aliases for bashmarks
    unalias l
    ";
  };

  services.udiskie.enable = true;

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "ag -g ''";
    defaultOptions = [
      "--color fg:223,hl:166,fg+:235,bg+:4,hl+:223"
      "--color info:246,prompt:223,spinner:142,pointer:166,marker:166 "
    ];
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  # home.file.".xmonad/xmonad.hs" = {
  #   source = dots + "xmonad/src/xmonad/xmonad.hs";
  # };

  # home.file.".config/dunst/dunstrc" = {
  #   source = dots + "dunst/src/dunstrc";
  # };

  # home.file.".config/qutebrowser" = {
  #   source = dots + "qutebrowser/src";
  #   recursive = true;
  # };

  # home.file.".emacs.d" = {
  #   source = dots + "emacs/src/emacs.d";
  #   recursive = true;
  # };

  # home.file.".ncmpcpp" = {
  #   source = dots + "ncmpcpp/src/ncmpcpp";
  #   recursive = true;
  # };

  # home.file.".config/ranger" = {
  #   source = dots + "ranger/src/ranger";
  #   recursive = true;
  # };

  # home.file.".config/zathura" = {
  #   source = dots + "zathura/src/zathura";
  #   recursive = true;
  # };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
