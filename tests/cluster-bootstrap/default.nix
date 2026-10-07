# The Cluster Bootstrap rehearsal's scratch network (FORGEJO_MIGRATION_PLAN.md
# -> Cluster Bootstrap). Not a check: it needs the internet (GitHub, the image
# registries), the real sealing keys and a dump, so it runs outside the
# sandbox through tests/cluster-bootstrap/run, which drives it.
#
# Two VMs on a private network, at the real hosts' addresses so every
# committed manifest applies unchanged:
#   nas 192.168.1.3  NFS exports at the real /pool-1 paths, empty except
#                    k8s/forgejo-backup (the dump); read-only git mirrors of
#                    the live repos over git:// (stand-in for the nas mirror);
#                    the only DNS resolver either VM uses, answering just the
#                    allowlist below
#   nuc 192.168.1.4  k3s with nuc's flags and sysctls
# Containment holds by construction, not by reviewing manifests: names
# outside the allowlist (Cloudflare's API, ACME, ntfy.sh, ...) do not
# resolve, and nuc drops every private and tailnet destination outside the
# test network, so nothing reaches the real nas, nuc or LAN. The real nas
# exports /pool-1 rw to `*` -- a scratch cluster on the LAN would write to
# live data.
{pkgs}: let
  # Image registries the manifests pull from (and their CDNs), and GitHub,
  # ArgoCD's source until the nas mirror. Subdomains included.
  allowed = import ./allowed.nix;
  # Both VMs: the user-mode NIC (eth0) for the internet, static so no DHCP
  # hands out slirp's resolver.
  uplink = {
    networking.useDHCP = false;
    networking.interfaces.eth0.ipv4.addresses = [
      {
        address = "10.0.2.15";
        prefixLength = 24;
      }
    ];
    networking.defaultGateway = {
      address = "10.0.2.2";
      interface = "eth0";
    };
  };
  # The real hosts' LAN addresses on the test VLAN (eth1), in place of the
  # driver's 192.168.1.<node number>.
  at = n: let
    address = "192.168.1.${toString n}";
  in {
    networking.interfaces.eth1.ipv4.addresses = pkgs.lib.mkForce [
      {
        inherit address;
        prefixLength = 24;
      }
    ];
    networking.primaryIPAddress = pkgs.lib.mkForce address;
  };
in
  pkgs.testers.runNixOSTest {
    name = "cluster-bootstrap";
    # The driver kills the VMs after an hour by default; run holds them up
    # for the whole rehearsal.
    globalTimeout = 24 * 3600;

    nodes.nas = {
      imports = [uplink (at 3) ../../role/garage.nix];
      # /pool-1 (every PV, the dump, Garage) lives on the root disk; the
      # default size filled up and postgres failed with ENOSPC.
      virtualisation.diskSize = 30000;
      virtualisation.fileSystems."/".autoResize = true;
      # Test-only credentials; the real nas's come from agenix.
      garageNode = {
        enable = true;
        dataDir = "/pool-1/garage/data";
        metadataDir = "/pool-1/garage/meta";
        capacity = "20G";
        secretsFile = pkgs.writeText "garage-test-secrets" ''
          GARAGE_RPC_SECRET=${builtins.hashString "sha256" "cluster-bootstrap rpc"}
          GARAGE_ADMIN_TOKEN=${builtins.hashString "sha256" "cluster-bootstrap admin"}
        '';
        keys.cnpg = pkgs.writeText "garage-test-cnpg-key" (builtins.toJSON {
          accessKeyId = "GK" + builtins.substring 0 24 (builtins.hashString "sha256" "cluster-bootstrap cnpg id");
          secretAccessKey = builtins.hashString "sha256" "cluster-bootstrap cnpg secret";
        });
        buckets.cnpg-backups = ["cnpg"];
      };
      networking.nameservers = ["127.0.0.1"];
      networking.firewall.enable = false;
      services.dnsmasq = {
        enable = true;
        resolveLocalQueries = false;
        settings = {
          no-resolv = true;
          listen-address = ["127.0.0.1" "192.168.1.3"];
          bind-interfaces = true;
          server = map (d: "/${d}/10.0.2.3") allowed;
          log-queries = true;
        };
      };
      services.nfs.server = {
        enable = true;
        exports = ''
          /pool-1/ *(rw,fsid=root,no_subtree_check,no_root_squash)
          /pool-1/k8s *(rw,no_subtree_check,nohide,no_root_squash)
          /pool-1/music *(rw,no_subtree_check,nohide,no_root_squash)
        '';
      };
      systemd.tmpfiles.rules =
        map (p: "d /pool-1/${p} 0777 root root -") [
          "k8s"
          "k8s/forgejo-backup"
          "k8s/immich-db-backup"
          "k8s/miniflux-db-backup"
          "k8s/pinepods-db-backup"
          "k8s/pinepods-downloads"
          "music"
          "cloud/photos"
          "mediastack/media/audiobooks"
          "mediastack/media/music"
          "vaultwarden"
        ]
        ++ ["d /srv/git 0755 root root -"];
      # The dump and the git mirrors come from the host (run) through the
      # driver's shared directory; NFS cannot re-export 9p, so the dump is
      # copied onto local disk before nfsd starts.
      virtualisation.sharedDirectories.rehearsal = {
        source = ''"$REHEARSAL_DIR"'';
        target = "/rehearsal";
      };
      systemd.services.rehearsal-dump = {
        wantedBy = ["multi-user.target"];
        before = ["nfs-server.service"];
        after = ["systemd-tmpfiles-setup.service"];
        unitConfig.RequiresMountsFor = "/rehearsal";
        serviceConfig.Type = "oneshot";
        script = ''
          cp /rehearsal/dump/forgejo-*.tar /pool-1/k8s/forgejo-backup/
          chown 1000:1000 /pool-1/k8s/forgejo-backup/*
        '';
      };
      systemd.services.git-mirror = {
        wantedBy = ["multi-user.target"];
        unitConfig.RequiresMountsFor = "/rehearsal";
        serviceConfig.ExecStart = "${pkgs.git}/bin/git daemon --reuseaddr --export-all --base-path=/rehearsal/git --listen=0.0.0.0 /rehearsal/git";
      };
      virtualisation.forwardPorts = [
        {
          from = "host";
          host.port = 19418;
          guest.port = 9418;
        }
      ];
    };

    nodes.nuc = {
      imports = [uplink (at 4)];
      networking.nameservers = ["192.168.1.3"];
      networking.firewall.enable = false;
      # New connections to every private and tailnet destination outside the
      # test network: the host (10.0.2.2), the real LAN, the tailnet. Replies
      # pass: the host's forwarded API port arrives from 10.0.2.2.
      networking.nftables = {
        enable = true;
        tables.contain = {
          family = "ip";
          content = ''
            set blocked {
              type ipv4_addr; flags interval
              elements = { 10.0.2.0/24, 100.64.0.0/10, 172.16.0.0/12, 192.168.0.0/16 }
            }
            chain egress {
              type filter hook output priority 0;
              ct state established,related accept
              fib daddr type local accept
              ip daddr 192.168.1.0/24 accept
              ip daddr 10.0.2.3 accept
              ip daddr @blocked drop
            }
            chain forwarded {
              type filter hook forward priority 0;
              ct state established,related accept
              fib daddr type local accept
              ip daddr 192.168.1.0/24 accept
              ip daddr @blocked drop
            }
          '';
        };
      };
      # As machines/nuc: blocky on :53, traefik and blocky on the (absent)
      # tailnet address.
      boot.kernel.sysctl."net.ipv4.ip_unprivileged_port_start" = 53;
      boot.kernel.sysctl."net.ipv4.ip_nonlocal_bind" = 1;
      boot.supportedFilesystems = ["nfs"];
      services.k3s = {
        enable = true;
        role = "server";
        token = "cluster-bootstrap-rehearsal";
        clusterInit = true;
        extraFlags = [
          "--disable=traefik"
          "--disable=servicelb"
          "--resolv-conf=/etc/k3s/resolv.conf"
          "--tls-san=127.0.0.1"
          # The LAN address, as on nuc; not slirp's 10.0.2.15.
          "--node-ip=192.168.1.4"
        ];
      };
      environment.etc."k3s/resolv.conf".text = "nameserver 192.168.1.3\n";
      virtualisation = {
        memorySize = 32768;
        cores = 12;
        diskSize = 150000;
        writableStoreUseTmpfs = false;
        forwardPorts = [
          {
            from = "host";
            host.port = 16443;
            guest.port = 6443;
          }
        ];
      };
    };

    # Boots the pair, hands the kubeconfig to the host, then holds the VMs up
    # until run (or the user) creates $REHEARSAL_DIR/stop.
    testScript = ''
      import os, time
      d = os.environ["REHEARSAL_DIR"]
      start_all()
      nas.wait_for_unit("nfs-server.service")
      nas.wait_for_unit("dnsmasq.service")
      nas.wait_for_unit("git-mirror.service")
      nas.wait_for_unit("garage-provision.service")
      nuc.wait_for_unit("k3s.service")
      nuc.wait_until_succeeds("k3s kubectl get node nuc | grep -w Ready", timeout=600)
      # Into the driver's output directory (run passes $REHEARSAL_DIR).
      nuc.succeed("sed 's|127.0.0.1:6443|127.0.0.1:16443|' /etc/rancher/k3s/k3s.yaml > /tmp/kubeconfig")
      nuc.copy_from_vm("/tmp/kubeconfig", "")
      open(os.path.join(d, "ready"), "w").close()
      while not os.path.exists(os.path.join(d, "stop")):
          time.sleep(10)
    '';
  }
