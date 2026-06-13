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
      # Bound the emacs.service cgroup. Subprocesses launched under the daemon
      # (LSP servers, terminals, build/test jobs, AI assistants) have their RAM
      # and page cache charged here; left unbounded, one can grow until it
      # triggers a system-wide OOM that kills the daemon along with everything
      # else. These limits keep any such blowup contained to this cgroup:
      #   MemoryHigh    - soft: reclaim (drops reclaimable page cache) + throttle.
      #   MemoryMax     - hard: a runaway is OOM-killed within this cgroup, where
      #                   the kernel targets the largest process rather than the
      #                   small daemon -- the session survives instead of the
      #                   whole machine OOMing.
      #   MemorySwapMax - bounds swap so a runaway can't thrash the system.
      # Sized for this host's RAM; revisit if the hardware changes.
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
