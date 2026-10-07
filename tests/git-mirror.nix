# checks.x86_64-linux.git-mirror: role/git-mirror.nix's guarantees, the ones
# the deploy source of truth rests on (FORGEJO_MIGRATION_PLAN.md -> Decisions
# -> nas `master`): master only moves forward and cannot be deleted; a read
# key fetches but cannot push; an unknown key gets nothing.
{
  pkgs,
  gitMirrorModule,
}: let
  # Test-only keys, generated at build time.
  keys = pkgs.runCommand "git-mirror-test-keys" {nativeBuildInputs = [pkgs.openssh];} ''
    mkdir $out
    for k in writer reader stranger; do
      ssh-keygen -q -t ed25519 -N "" -C "$k" -f $out/$k
    done
  '';
  pub = k: builtins.readFile "${keys}/${k}.pub";
in
  pkgs.testers.runNixOSTest {
    name = "git-mirror";

    nodes.nas = {
      imports = [gitMirrorModule];
      services.openssh.enable = true;
      gitMirror = {
        enable = true;
        root = "/srv/git";
        writeKeys = [(pub "writer")];
        readKeys = [(pub "reader")];
      };
    };

    nodes.client = {
      environment.systemPackages = [pkgs.git];
    };

    testScript = ''
      start_all()
      nas.wait_for_unit("git-mirror-init.service")
      nas.wait_for_unit("sshd.service")
      for key in ("denyNonFastForwards", "denyDeletes"):
          nas.succeed(f"test \"$(${pkgs.git}/bin/git -c safe.directory='*' -C /srv/git/dots.git config receive.{key})\" = true")

      client.succeed(
        "mkdir -p /root/.ssh && cp ${keys}/writer ${keys}/reader ${keys}/stranger /root/.ssh/ && chmod 600 /root/.ssh/*",
        "ssh-keyscan nas > /root/.ssh/known_hosts 2>/dev/null",
        "git config --global user.email t@t && git config --global user.name t",
        "git init -q -b master /work && cd /work && git commit -q --allow-empty -m one && git commit -q --allow-empty -m two",
      )
      def git(key, cmd):
          return f"cd /work && GIT_SSH_COMMAND='ssh -i /root/.ssh/{key} -o IdentitiesOnly=yes -o BatchMode=yes' git {cmd}"
      # The absolute form real clients use (ssh:// paths are not home-relative).
      url = "ssh://git@nas/srv/git/dots.git"

      with subtest("the write key pushes master"):
          client.succeed(git("writer", f"push {url} master"))

      with subtest("master cannot move backwards"):
          out = client.fail(git("writer", f"push --force {url} HEAD~1:master") + " 2>&1")
          assert "non-fast-forward" in out or "denying" in out, out

      with subtest("master cannot be deleted"):
          out = client.fail(git("writer", f"push {url} :master") + " 2>&1")
          assert "deletion" in out or "denying" in out, out

      with subtest("the read key fetches"):
          client.succeed(f"GIT_SSH_COMMAND='ssh -i /root/.ssh/reader -o IdentitiesOnly=yes -o BatchMode=yes' git ls-remote {url} | grep -q refs/heads/master")
          client.succeed(f"GIT_SSH_COMMAND='ssh -i /root/.ssh/reader -o IdentitiesOnly=yes -o BatchMode=yes' git clone -q {url} /clone")

      with subtest("the read key cannot push"):
          client.succeed("cd /work && git commit -q --allow-empty -m three")
          out = client.fail(git("reader", f"push {url} master") + " 2>&1")
          assert "read-only" in out, out

      with subtest("the read key gets no shell"):
          client.fail("ssh -i /root/.ssh/reader -o IdentitiesOnly=yes -o BatchMode=yes git@nas true")

      with subtest("an unknown key gets nothing"):
          client.fail(git("stranger", f"ls-remote {url}"))

      with subtest("the forward push still works"):
          client.succeed(git("writer", f"push {url} master"))
    '';
  }
