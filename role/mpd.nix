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
    device = "10.0.100.3:/pool-1/music";
    options = ["nfsvers=3" "noatime" "nodiratime" "rsize=32768" "async" "ro"];
  };

  home-manager.users.ftzm = {
    services.mpd = {
      enable = true;
      dbFile = null;
      musicDirectory = "/mnt/music";
      extraConfig = ''
        database {
            plugin  "proxy"
            host    "10.0.100.4"
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
