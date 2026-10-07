{
  config,
  pkgs,
  ...
}: {
  age.secrets.k3s = {
    file = ../../secrets/k3s.age;
  };
  networking.firewall.allowedTCPPorts = [
    6443 # k3s: required so that pods can reach the API server (running on port 6443 by default)
    # 2379 # k3s, etcd clients: required if using a "High Availability Embedded etcd" configuration
    # 2380 # k3s, etcd peers: required if using a "High Availability Embedded etcd" configuration
  ];
  networking.firewall.allowedUDPPorts = [
    # 8472 # k3s, flannel: required if using multi-node for inter-node networking
  ];
  # nix-built images for the cluster, imported into k3s and pinned
  # (role/k3s-local-images.nix): the Forgejo OpenTofu job's
  # (cluster/lib/forgejo-tofu.libsonnet).
  imports = [../../role/k3s-local-images.nix];
  k3sLocalImages.forgejo-tofu = {
    image = pkgs.callPackage ../../pkgs/forgejo-tofu-image.nix {};
    ref = "localhost/forgejo-tofu:nix";
  };

  services.k3s = {
    enable = true;
    role = "server";
    tokenFile = "${config.age.secrets.k3s.path}";
    clusterInit = true;
    extraFlags = [
      "--disable=traefik"
      "--disable=servicelb"
      "--resolv-conf=/etc/k3s/resolv.conf"
    ];
  };

  # Cluster DNS must not follow the host's /etc/resolv.conf: tailscale rewrites
  # it at runtime (nameserver 100.100.100.100, search tail.ftzmlab.xyz). The
  # kubelet copies its search domains into every new pod, and CoreDNS takes its
  # upstream from whatever the file held when CoreDNS started. With the
  # tail.ftzmlab.xyz search domain, ndots:5 lookups like
  # cleanuparr.media.svc.cluster.local hit the public *.ftzmlab.xyz wildcard
  # first and resolve to the WAN IP. lan.ftzmlab.xyz is forwarded to blocky by
  # the coredns-custom ConfigMap (cluster/environments/lab/lab.jsonnet).
  environment.etc."k3s/resolv.conf".text = ''
    nameserver 192.168.1.1
  '';
}
