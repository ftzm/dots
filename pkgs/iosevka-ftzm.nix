# The custom Iosevka build, prebuilt: github.com/ftzm/iosevka-ftzm builds it
# from its own pinned nixpkgs and attaches the TTFs to each release. Fetched
# by URL and hash -- no host or CI here ever builds the font (or the node
# toolchain it needs). Renovate bumps `version` and nix-update the hash.
{
  stdenvNoCC,
  fetchzip,
}:
stdenvNoCC.mkDerivation rec {
  pname = "iosevka-ftzm";
  version = "34.7.0-1";

  src = fetchzip {
    url = "https://github.com/ftzm/iosevka-ftzm/releases/download/v${version}/iosevka-ftzm.tar.xz";
    hash = "sha256-B/HuO7b5Z345OLgzGn+hI7iYuNBx+QfmTmPEa18rSbg=";
    stripRoot = false;
  };

  installPhase = ''
    runHook preInstall
    install -Dm444 -t $out/share/fonts/truetype $src/*.ttf
    runHook postInstall
  '';
}
