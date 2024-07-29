{pkgs, ...}: let
  patched-ucm = pkgs.alsa-ucm-conf.overrideAttrs (old: rec {
    patches = [
      (pkgs.fetchpatch {
        # TODO: Remove this patch in the next package upgrade
        name = "rt1318-fix-one.patch";
        url = "https://github.com/alsa-project/alsa-ucm-conf/commit/7e22b7c214d346bd156131f3e6c6a5900bbf116d.patch";
        hash = "sha256-5X0ANXTSRnC9jkvMLl7lA5TBV3d1nwWE57DP6TwliII=";
      })
      (pkgs.fetchpatch {
        # TODO: Remove this patch in the next package upgrade
        name = "rt1318-fix-two.patch";
        url = "https://github.com/alsa-project/alsa-ucm-conf/commit/4e0fcc79b7d517a957e12f02ecae5f3c69fa94dc.patch";
        hash = "sha256-cuZPEEqb8+d1Ak2tA+LVEh6gtGt1X+LiAnfFYMIDCXY=";
      })
    ];
  });
in {
  environment.sessionVariables.ALSA_CONFIG_UCM2 = "${patched-ucm}/share/alsa/ucm2";
}
