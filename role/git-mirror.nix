# The nas mirror (FORGEJO_MIGRATION_PLAN.md -> Decisions -> nas `master` is
# the deploy source of truth): bare repos served by this host's own sshd to a
# `git` user whose login shell is git-shell -- no dependency on k3s, traefik,
# cert-manager or nuc. Forward-only: receive.denyNonFastForwards and
# receive.denyDeletes, so a stale Forgejo (restored dump, force-push) has its
# push refused instead of rewinding what ArgoCD and the fleet writer deploy.
#
# Write keys push (Forgejo's push mirror; the workstation, for fixes during a
# Forgejo outage). Read keys (the fleet writer, ArgoCD) may only fetch: a
# forced command admits git-upload-pack and nothing else.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.gitMirror;
  readOnly = pkgs.writeShellScript "git-mirror-read-only" ''
    case "$SSH_ORIGINAL_COMMAND" in
      "git-upload-pack "*) exec ${pkgs.git}/bin/git-shell -c "$SSH_ORIGINAL_COMMAND" ;;
      *) echo "git-mirror: this key is read-only" >&2; exit 1 ;;
    esac
  '';
in {
  options.gitMirror = {
    enable = lib.mkEnableOption "the forward-only git mirror";
    root = lib.mkOption {
      type = lib.types.str;
      default = "/pool-1/git";
      description = "Directory holding the bare repos, served as ssh://git@<host>/<repo>.git.";
    };
    repos = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["dots"];
    };
    writeKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Public keys that may push.";
    };
    readKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Public keys that may only fetch (git-upload-pack).";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.git = {
      isSystemUser = true;
      group = "git";
      home = cfg.root;
      shell = "${pkgs.git}/bin/git-shell";
      openssh.authorizedKeys.keys =
        map (k: "restrict ${k}") cfg.writeKeys
        # git-shell (the login shell) runs a forced command itself and admits
        # only git commands or ~/git-shell-commands/<name>: hence `read-only`.
        ++ map (k: ''restrict,command="read-only" ${k}'') cfg.readKeys;
    };
    users.groups.git = {};

    # Creates the root and missing repos, (re)asserts the forward-only
    # settings and the read-only command on every boot and deploy, so they
    # cannot drift. Root, not tmpfiles: systemd-tmpfiles refuses to create
    # entries below a directory an unprivileged user owns.
    systemd.services.git-mirror-init = {
      description = "git mirror repos";
      wantedBy = ["multi-user.target"];
      after = ["local-fs.target"];
      restartTriggers = [(builtins.toJSON cfg.repos)];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      path = [pkgs.git pkgs.util-linux];
      script =
        ''
          install -d -m 0750 -o git -g git ${cfg.root}
          install -d -m 0755 -o root -g root ${cfg.root}/git-shell-commands
          ln -sfn ${readOnly} ${cfg.root}/git-shell-commands/read-only
        ''
        + lib.concatMapStrings (r: ''
          repo=${cfg.root}/${r}.git
          [ -d "$repo" ] || runuser -u git -- git init --quiet --bare --initial-branch=master "$repo"
          runuser -u git -- git -C "$repo" config receive.denyNonFastForwards true
          runuser -u git -- git -C "$repo" config receive.denyDeletes true
        '')
        cfg.repos;
    };
  };
}
