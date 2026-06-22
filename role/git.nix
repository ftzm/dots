{pkgs, ...}: {
  hm.home.packages = [pkgs.gh]; # github cli (gh repo create, etc.)
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
      github = {
        user = "ftzm";
      };
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
      # Let git authenticate pushes via the gh CLI's stored token. Mirrors
      # `gh auth setup-git`: empty helper clears inherited ones, then gh's.
      # (Can't be set by `gh auth setup-git` at runtime -- ~/.config/git is
      # the read-only home-manager-managed store path.)
      credential = {
        "https://github.com".helper = ["" "${pkgs.gh}/bin/gh auth git-credential"];
        "https://gist.github.com".helper = ["" "${pkgs.gh}/bin/gh auth git-credential"];
      };
    };
  };
}
