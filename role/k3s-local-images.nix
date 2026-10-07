# Container images built by nix, imported into this host's k3s (containerd,
# namespace k8s.io) instead of pulled from a registry. k3s itself imports
# tarballs only when it starts, so each image gets a oneshot that imports it
# whenever it changes and pins it (io.cri-containerd.pinned), so the
# kubelet's image GC cannot remove it between uses. Pods use the image's
# name with imagePullPolicy: Never.
{
  config,
  lib,
  ...
}: let
  cfg = config.k3sLocalImages;
in {
  options.k3sLocalImages = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        image = lib.mkOption {
          type = lib.types.package;
          description = "A dockerTools.streamLayeredImage (a script writing the image tar to stdout).";
        };
        ref = lib.mkOption {
          type = lib.types.str;
          description = "The image reference it carries, e.g. localhost/forgejo-tofu:nix.";
        };
      };
    });
    default = {};
  };

  config.systemd.services = lib.mapAttrs' (name: img:
    lib.nameValuePair "k3s-image-${name}" {
      description = "Import ${img.ref} into k3s";
      after = ["k3s.service"];
      requires = ["k3s.service"];
      wantedBy = ["multi-user.target"];
      restartTriggers = [img.image];
      path = [config.services.k3s.package];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        until k3s ctr version >/dev/null 2>&1; do sleep 2; done
        ${img.image} | k3s ctr -n k8s.io images import -
        k3s ctr -n k8s.io images label ${img.ref} io.cri-containerd.pinned=pinned
      '';
    })
  cfg;
}
