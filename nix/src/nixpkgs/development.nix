{
  programs.git = {
    enable = true;
    ignores = [
      # Emacs
      "**/*~"
      "**/*#"
      ".mypy_cache"
      ".direnv"
      # Haskell
      "*.hi"
      "*.o"
      # Nix
      "result/*"
    ];
    userEmail = "matthew@fitzsimmons.io";
    userName = "ftzm";
    extraConfig = {
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
    };
  };
}
