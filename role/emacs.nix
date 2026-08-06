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
  # Weekly controlled Elpaca update: fetch + merge + rebuild every package, then
  # re-pin the lockfile ONLY if the result is clean (see ftzm/elpaca-update-and-relock
  # in init.el). A broken upstream leaves the committed pins intact and rolls back
  # on restart. Result is surfaced as a desktop notification to review + commit.
  elpacaUpdateScript = pkgs.writeShellScript "elpaca-update" ''
    set -u
    summary="$(${emacs}/bin/emacsclient --eval '(ftzm/elpaca-update-and-relock)' 2>&1 \
      | ${pkgs.gnused}/bin/sed -e 's/^"//' -e 's/"$//' | tail -1)"
    ${pkgs.libnotify}/bin/notify-send -a Emacs "Emacs package update" "$summary" || true
    echo "$summary"
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

  systemd.user.services.elpaca-update = {
    description = "Update Emacs (Elpaca) packages and re-pin lockfile";
    requires = ["emacs.service"];
    after = ["emacs.service"];
    serviceConfig = {
      Type = "oneshot";
      # A full fetch + merge + rebuild of every package can take several minutes.
      TimeoutStartSec = "30min";
      ExecStart = "${elpacaUpdateScript}";
    };
  };

  systemd.user.timers.elpaca-update = {
    description = "Weekly Emacs package update";
    timerConfig = {
      # Off-hours so the mid-session rebuild never disrupts active work.
      OnCalendar = "Mon 04:00";
      Persistent = true;
    };
    wantedBy = ["timers.target"];
  };
}
