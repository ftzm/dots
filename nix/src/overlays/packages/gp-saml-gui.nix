{ fetchFromGitHub
, glib
, glib-networking
, gobjectIntrospection
, gtk3
, python3
, webkitgtk
, wrapGAppsHook
}:

let
  version = "1.0";
in python3.pkgs.buildPythonApplication rec {
  name = "gp-saml-gui-${version}";

  src = fetchFromGitHub {
    owner = "dlenski";
    repo = "gp-saml-gui";
    rev = "6133ffeb7a47a12afff2e7eb404434d24ddc01e1";
    sha256 = "17nimg5nyqjw82lmr2vaj84dn7xvlb8mvyg74bid32hk4jlkni4w";
  };

  doCheck = false;
  buildPhase = null;
  dontUseSetuptoolsBuild = true;
  dontUsePipInstall = true;
  strictDeps = false;

  propagatedBuildInputs = [
    glib
    glib-networking
    python3
    python3.pkgs.pygobject3
    python3.pkgs.requests
  ];

  buildInputs = [
    wrapGAppsHook
    webkitgtk
  ];

  nativeBuildInputs = [
    gobjectIntrospection # populate GI_TYPELIB_PATH
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp gp-saml-gui.py $out/bin/
  '';
}
