{pkgs, ...}: let
  package = pkgs.emacs-pgtk.override {};
  epkgs = pkgs.emacsPackagesFor package;
  # tree-sitter bundle with a Chez-corrected scheme grammar (see emacs-grammars.nix)
  schemeGrammars = import ./emacs-grammars.nix {inherit pkgs epkgs;};
  emacs = epkgs.emacsWithPackages (e: [
    schemeGrammars
    #e.tree-sitter
    #e.tree-sitter-langs
    #e.telega
    e.vterm
    #e.emms
    #emms-taglib
    #e.org
    #e.pdf-tools
  ]);
  archiveScript = pkgs.writeShellScript "org-archive" ''
    ${emacs}/bin/emacsclient --eval "(ftzm/org-archive-old-tasks)"
  '';
in {
  environment.systemPackages = [
    emacs
    pkgs.python3
    pkgs.emacs-lsp-booster
  ];

  systemd.user.services.emacs = {
    description = "Emacs daemon";
    # Don't kill a running daemon on `switch` (incl. comin auto-deploys).
    # Any change to the emacs derivation changes ExecStart's store path,
    # which would otherwise stop+restart the daemon mid-session. The new
    # version is picked up on reboot or `systemctl --user restart emacs`.
    restartIfChanged = false;
    serviceConfig = {
      Type = "simple";
      ExecStart = "${emacs}/bin/emacs --fg-daemon";
      Restart = "on-failure";
      TimeoutStopSec = 10;
      Environment = [
        "COLORTERM=truecolor"
        "PATH=/etc/profiles/per-user/%u/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin"
      ];
      # Bound the whole emacs.service cgroup so a blowup can't take the session
      # down. claudemacs runs builds/tests (cwasm-s5: chez, js140, node) as Emacs
      # subprocesses, so their RAM + page cache is charged here. On 2026-06-13
      # this cgroup hit 60G + 41G swap and triggered a *system-wide* OOM that
      # killed the session. These limits contain it:
      #   MemoryHigh   - soft: kernel reclaims (drops reclaimable page cache,
      #                  most of the bloat) and throttles before trouble.
      #   MemoryMax    - hard: a real runaway is OOM-killed *inside* this cgroup;
      #                  the killer picks the largest process (the multi-GB
      #                  build), so the ~350M emacs daemon survives, and it never
      #                  escalates to a system-wide OOM.
      #   MemorySwapMax- caps the swap-thrash that preceded the original OOM.
      # Sized for this host's 62G RAM / 68G swap, leaving system headroom.
      MemoryHigh = "24G";
      MemoryMax = "40G";
      MemorySwapMax = "8G";
    };
    wantedBy = ["default.target"];
  };

  systemd.user.services.org-archive = {
    description = "Archive old done org-mode tasks";
    requires = ["emacs.service"];
    after = ["emacs.service"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${archiveScript}";
    };
  };

  systemd.user.timers.org-archive = {
    description = "Daily org-mode archive";
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
    wantedBy = ["timers.target"];
  };
}
