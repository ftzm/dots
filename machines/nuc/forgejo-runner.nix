# Forgejo Actions runner, isolated in a microVM on nuc.
#
# The runner *daemon* is trusted; the *jobs* it launches are untrusted code, so
# they must not share a kernel with the k3s cluster running on nuc. This guest
# gives them a real VM boundary. Networking is qemu user-mode (SLIRP): NAT'd
# outbound only, no host bridge, eno1 untouched, inbound closed — the runner
# only ever dials out to the Forgejo instance.
#
# Version is Renovate-driven (see the custom manager in cluster/renovate.jsonnet
# targeting this file) and deployed by comin, matching the rest of the fleet.
{
  pkgs,
  inputs,
  lib,
  ...
}: let
  # Renovate bumps this tag via the runnerImageManager in cluster/renovate.jsonnet
  # (regex-matches this assignment); comin then deploys the update.
  runnerImage = "code.forgejo.org/forgejo/runner:12.13.1";
  instanceUrl = "https://forgejo.lan.ftzmlab.xyz";
  runnerName = "nuc-microvm";
  # Jobs run as containers in the guest's own podman (contained in the VM).
  runnerLabels = "docker:docker://node:22-bookworm,ubuntu-latest:docker://node:22-bookworm";
  # Dedicated host dir for the decrypted token — we share ONLY this into the
  # guest, never all of /run/agenix.
  tokenHostDir = "/run/forgejo-runner-secret";
in {
  imports = [inputs.microvm.nixosModules.host];

  # Decrypt the runner registration token on nuc into its own dir.
  age.secrets.forgejo-runner-token = {
    file = ../../secrets/forgejo-runner-token.age;
    path = "${tokenHostDir}/token";
    mode = "0444"; # readable by virtiofs → guest; token is low-value (registration only)
  };

  microvm.vms.forgejo-runner = {
    config = {
      system.stateVersion = "25.11";
      networking.hostName = "forgejo-runner";

      microvm = {
        hypervisor = "qemu";
        vcpu = 2;
        mem = 4096;

        # Outbound-only NAT. No tap/bridge on the host, so eno1 is never touched.
        interfaces = [
          {
            type = "user";
            id = "usernet";
            mac = "02:00:00:00:07:01";
          }
        ];

        shares = [
          {
            # Share the host nix store read-only (standard microvm pattern).
            source = "/nix/store";
            mountPoint = "/nix/.ro-store";
            tag = "ro-store";
            proto = "virtiofs";
          }
          {
            # Only the runner token, read-only.
            source = tokenHostDir;
            mountPoint = tokenHostDir;
            tag = "runner-token";
            proto = "virtiofs";
          }
        ];

        # Persist the runner registration (.runner) and the runner's own state.
        volumes = [
          {
            image = "forgejo-runner-data.img";
            mountPoint = "/var/lib/forgejo-runner";
            size = 20480; # MiB
          }
        ];
      };

      # qemu user networking hands the guest 10.0.2.15/24 via its built-in DHCP.
      networking.useNetworkd = true;
      systemd.network.networks."10-usernet" = {
        matchConfig.MACAddress = "02:00:00:00:07:01";
        networkConfig.DHCP = "yes";
      };
      # Resolve the instance's LAN name + pull images.
      networking.firewall.enable = false; # guest is already NAT-isolated

      # Podman for job containers, entirely inside the VM — matching the rest of
      # the fleet (nuc's host backend is podman too). The runner's `docker://`
      # backend speaks the Docker API, which podman's socket serves, so no docker
      # daemon is needed. dockerSocket.enable exposes it at /run/docker.sock.
      virtualisation.podman = {
        enable = true;
        dockerSocket.enable = true;
        autoPrune.enable = true; # job containers churn; reclaim space
      };

      # One-time registration: create /var/lib/forgejo-runner/.runner from the
      # token if it doesn't exist yet. Runs the pinned image so the register
      # binary matches the daemon. podman run needs no persistent daemon.
      #
      # --user 0:0: the image runs as uid 1000, but /data (the volume) is
      # root-owned and the daemon must also drive the root:podman podman.sock —
      # so both containers run as root. The microVM is the trust boundary here,
      # not the in-guest uid, so a root runner inside this single-purpose VM is
      # the coherent choice.
      systemd.services.forgejo-runner-register = {
        description = "Register forgejo-runner (once)";
        wantedBy = ["multi-user.target"];
        after = ["network-online.target"];
        wants = ["network-online.target"];
        before = ["podman-forgejo-runner.service"];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          # The guest has no interactive shell; mirror this unit's output to the
          # serial console so it lands in the host journal (microvm@…) for debugging.
          StandardOutput = "journal+console";
          StandardError = "journal+console";
        };
        script = ''
          set -eux
          if [ ! -f /var/lib/forgejo-runner/.runner ]; then
            ${pkgs.podman}/bin/podman run --rm \
              --user 0:0 \
              -v /var/lib/forgejo-runner:/data \
              ${runnerImage} \
              /bin/forgejo-runner register --no-interactive \
              --instance ${instanceUrl} \
              --token "$(cat ${tokenHostDir}/token)" \
              --name ${runnerName} \
              --labels '${runnerLabels}'
          fi
        '';
      };

      virtualisation.oci-containers = {
        backend = "podman";
        containers.forgejo-runner = {
          image = runnerImage;
          # .runner + config live in /data; the guest's podman socket (exposed at
          # the docker-compatible path) lets the runner spawn job containers as
          # siblings (inside this VM only).
          volumes = [
            "/var/lib/forgejo-runner:/data"
            "/run/podman/podman.sock:/var/run/docker.sock"
          ];
          # Image has no entrypoint (Cmd = /bin/forgejo-runner), so name the
          # binary explicitly — a bare "daemon" would be exec'd as a command.
          cmd = ["/bin/forgejo-runner" "daemon"];
          # --user=0:0: read the root-owned .runner and drive the root:podman
          # podman.sock (see the register service for the rationale).
          extraOptions = ["--pull=always" "--user=0:0"];
        };
      };

      # The daemon needs a successful registration (.runner) and the podman API
      # socket live before it starts; Restart=always keeps it up for heavy jobs.
      systemd.services."podman-forgejo-runner" = {
        requires = ["podman.socket" "forgejo-runner-register.service"];
        after = ["podman.socket" "forgejo-runner-register.service"];
        serviceConfig.Restart = lib.mkForce "always";
      };
    };
  };
}
