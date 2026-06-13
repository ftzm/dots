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
  # Periodic memory snapshot. On 2026-06-13 the emacs.service *cgroup* ballooned
  # to ~60G + 41G swap over ~2 days and tripped the OOM killer; the cause
  # couldn't be pinned because the process was already gone. Investigation
  # showed Emacs's own Lisp heap was tiny (~78 MiB) -- the bulk was child
  # processes in the cgroup (claudemacs spawns `claude' CLIs, ~500M each).
  # So this logs BOTH signals on a timer: the cgroup's per-process RSS (catches
  # child-process blowups) and Emacs's `memory-report' (catches Lisp leaks).
  memReportScript = pkgs.writeShellApplication {
    name = "emacs-memory-report";
    runtimeInputs = [pkgs.coreutils pkgs.systemd emacs];
    text = ''
      log="''${XDG_STATE_HOME:-$HOME/.local/state}/emacs-memory-report.log"
      mkdir -p "$(dirname "$log")"
      ts=$(date -Iseconds)
      mem=$(systemctl --user show emacs.service -p MemoryCurrent --value 2>/dev/null || echo "?")
      peak=$(systemctl --user show emacs.service -p MemoryPeak --value 2>/dev/null || echo "?")
      {
        echo "==== $ts  cgroup MemoryCurrent=$mem MemoryPeak=$peak bytes ===="
        # Per-process RSS within the emacs.service cgroup. RSS via statm
        # (field 2 = pages; *4 = KiB) to avoid an awk dependency.
        cg=$(systemctl --user show emacs.service -p ControlGroup --value 2>/dev/null || echo "")
        procs="/sys/fs/cgroup''${cg}/cgroup.procs"
        if [ -r "$procs" ]; then
          echo "-- cgroup processes by RSS (KiB) --"
          while read -r pid; do
            [ -r "/proc/$pid/statm" ] || continue
            pages=$(cut -d' ' -f2 "/proc/$pid/statm" 2>/dev/null || echo 0)
            cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null | cut -c1-80)
            printf '%10d  %s  %s\n' "$(( pages * 4 ))" "$pid" "$cmd"
          done < "$procs" | sort -rn | head -15 || true
        fi
      } >> "$log"
      # Emacs Lisp-heap report: emacs writes the file itself (no shell-side
      # newline escaping) and kills its own *Memory Report* buffer so the
      # logger can't leak.
      if ! emacsclient --eval \
          "(let ((f \"$log\")) (memory-report) (with-current-buffer \"*Memory Report*\" (write-region (point-min) (point-max) f t) (kill-buffer)) t)" \
          >/dev/null 2>&1; then
        echo "  (emacsclient unavailable)" >> "$log"
      fi
      echo >> "$log"
      # keep the log bounded (~last 20k lines, several days of snapshots)
      lines=$(wc -l < "$log")
      if [ "$lines" -gt 20000 ]; then
        tail -n 20000 "$log" > "$log.tmp" && mv "$log.tmp" "$log"
      fi
    '';
  };
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

  systemd.user.services.emacs-memory-report = {
    description = "Snapshot Emacs daemon memory usage to a log";
    after = ["emacs.service"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${memReportScript}/bin/emacs-memory-report";
      # memory-report walks every buffer/variable; on a bloated, swap-thrashing
      # daemon that can be slow, but that snapshot is the one we most want.
      TimeoutStartSec = 180;
    };
  };

  systemd.user.timers.emacs-memory-report = {
    description = "Periodic Emacs memory snapshot";
    timerConfig = {
      OnBootSec = "10min";
      OnUnitActiveSec = "20min";
    };
    wantedBy = ["timers.target"];
  };
}
