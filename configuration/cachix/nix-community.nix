
{
  nix = {
    settings.substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://undo-foundation.cachix.org"
      "https://cache.zw3rk.com"
    ];
    settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "undo-foundation.cachix.org-1:BSP9SjfX89JXxs2QXF9qxTYBlTSG3ad4N7V4HSlH9s0="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
