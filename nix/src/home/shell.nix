{ pkgs, config, ... }:

let
  vterm_printf = builtins.readFile ./vterm_printf.sh;
  vterm_prompt_end = builtins.readFile ./vterm_prompt_end.sh;
in
{
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
      export GUIPASS=${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass
      # unalias conflicting aliases for bashmarks
      unalias l
      [[ -f ~/.falcon ]] && source ~/.falcon
      # Go
      export GOPATH=$HOME/.go
      export PATH=$GOPATH/bin:$PATH
      ${vterm_printf}
      ${vterm_prompt_end}

      alias groot='cd $(git rev-parse --show-toplevel)'

    '' + config.personal.zsh_extra;
  };
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
    nix-direnv.enable = true;
  };
}
