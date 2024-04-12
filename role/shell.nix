{
  pkgs,
  lib,
  config,
  ...
}: let
  emacs-vterm-bash = builtins.readFile ./emacs-vterm-bash.sh;
in {
  # options.personal.zsh_extra = lib.mkOption {
  #   type = lib.types.str;
  #   default = "";
  #   description = ''
  #     Extra lines added to zshrc.
  #   '';
  # };

  config.home-manager.users.ftzm = {
    # programs.zsh = {
    #   enable = false;
    #   # plugins = [
    #   #   {
    #   #     name = "bashmarks";
    #   #     file = "bashmarks.sh";
    #   #     src = pkgs.fetchFromGitHub {
    #   #       owner = "huyng";
    #   #       repo = "bashmarks";
    #   #       rev = "342169ad48954ac24cb58ef479a6d3238e90f42f";
    #   #       sha256 = "1p7yd0vd8his8lf8jzn8xiv3cwiw0yswk94744jk7pdn5znfv5gp";
    #   #     };
    #   #   }
    #   #   {
    #   #     name = "zsh-syntax-highlighting";
    #   #     src = pkgs.fetchFromGitHub {
    #   #       owner = "zsh-users";
    #   #       repo = "zsh-syntax-highlighting";
    #   #       rev = "650dd79d86f885f8802732e3748d8719e787d22f";
    #   #       sha256 = "11hynfzyv9ri7rjymj1ix016r8yqx2am692bjhq6qqdii6l8m40r";
    #   #     };
    #   #   }
    #   # ];
    #   oh-my-zsh = {
    #     # enable = true;
    #     plugins = ["vi-mode"];
    #     theme = "robbyrussell";
    #   };
    #   # enableAutosuggestions = true;
    #   # enableCompletion = true;
    #   shellAliases = {
    #     ".." = "cd ..";
    #     ll = "ls -l";
    #     k = "kubectl";
    #   };
    #   # history.ignoreDups = true;
    #   # history.extended = true;
    #   initExtra =
    #     ''
    #       # for profilgin
    #       zmodload zsh/zprof

    #       EDITOR='vim'
    #       export PATH=$HOME/bin:$HOME/.local/bin:$PATH
    #       # Ask for passwords in cli
    #       unset SSH_ASKPASS
    #       unset GIT_ASKPASS
    #       export GUIPASS=${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass
    #       # unalias conflicting aliases for bashmarks
    #       # unalias l
    #       # Go
    #       export GOPATH=$HOME/.go
    #       export PATH=$GOPATH/bin:$PATH

    #       alias groot='cd $(git rev-parse --show-toplevel)'

    #     ''
    #     + config.personal.zsh_extra;
    # };
    programs.bash = {
      enable = true;
      bashrcExtra = ''

        export SHELL=bash

        # export PS1='\[\e[1;$(($?==0?32:91))m\]\w\a\] $ \[\e[0m\]'

        # write to history immediately
        # shopt -s histappend
        # export PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'history -a'

        EDITOR='vim'
        export PATH=$HOME/bin:$HOME/.local/bin:$PATH

        # Ask for passwords in cli
        unset SSH_ASKPASS
        unset GIT_ASKPASS
        export GUIPASS=${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass

        # ${emacs-vterm-bash}
      '';
    };
    # programs.fzf = {
    #   enable = true;
    #   enableZshIntegration = true;
    #   enableBashIntegration = true;
    #   defaultCommand = "ag -g ''";
    #   defaultOptions = [
    #     "--color fg:223,hl:166,bg+:#282828,fg+:223,hl+:#fabd2f"
    #     "--color info:#504945,prompt:223,spinner:142,pointer:166,marker:166 "
    #   ];
    # };
    programs.starship = {
      enable = true;
      enableBashIntegration = true;
      settings = {
        add_newline = false;
        format = lib.concatStrings [
          "$directory"
          "$cmd_duration"
          "$line_break"
          "$character"
        ];
        scan_timeout = 10;
        character = {
          success_symbol = "[\\$](green)";
          error_symbol = "[\\$](red)";
        };
      };
    };
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
