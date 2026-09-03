{pkgs, ...}: {
  imports = [./nfs-automount.nix];

  environment.systemPackages = with pkgs; [
    nfs-utils
    libnfs
    ncmpcpp
  ];

  # for rpc-statd for nfs client: https://github.com/NixOS/nixpkgs/issues/76671
  services = {
    rpcbind.enable = true;
    nfs.server.enable = true;
  };

  # Mount music from nas
  nfsAutomounts."/mnt/music" = {
    device = "nas.tail.ftzmlab.xyz:/music";
    options = [
      "nfsvers=4.2" # session recovery across sleep / network change
      "soft"
      "timeo=100"
      "retrans=3" # fail in ~30s, never hang; safe because ro
      "ro"
      "noatime"
      "nodiratime"
      "rsize=1048576"
      "wsize=1048576"
      "actimeo=3600"
      "nconnect=4"
    ];
  };

  home-manager.users.ftzm = {
    services.mpd = {
      enable = true;
      dbFile = null;
      musicDirectory = "/mnt/music";
      extraConfig = ''
        database {
            plugin  "proxy"
            host    "nuc.tail.ftzmlab.xyz"
            port    "6600"
        }
        audio_output {
          type "pulse"
          name "Pulseaudio"
        }

      '';
    };

    # interact with mpd via playerctl
    services.mpdris2 = {enable = true;};
  };
}
