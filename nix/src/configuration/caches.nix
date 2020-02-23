let
  caches = [
    {
      url = "https://cache.dhall-lang.org";
      key = "cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM=";
      description = "Dhall Language";
    }
  ];
in {
  nix = {
    binaryCaches = map (x: x.url) caches;
    binaryCachePublicKeys = map (x: x.key) caches;
  };
}
