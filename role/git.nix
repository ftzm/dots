{
  hm.programs.git = {
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
    settings = {
      user = {
        email = "m@ftzm.org";
        name = "ftzm";
      };
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
    };
  };
}
