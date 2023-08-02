{
  programs.git = {
    enable = true;
    ignores = [
      # Emacs
      "**/*~"
      "**/*#"
      # Python
      ".mypy_cache"
      # direnv
      ".direnv"
      # Haskell
      "*.hi"
      "*.o"
      # Nix
      "result"
    ];
    userEmail = "m@ftzm.org";
    userName = "ftzm";
    extraConfig = {
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
    };
  };
}
