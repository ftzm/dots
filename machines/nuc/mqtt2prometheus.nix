{
  fetchFromGitHub,
  buildGoModule,
  lib,
}:
buildGoModule rec {
  pname = "mqtt2prometheus";
  version = "0.1.7";

  src = fetchFromGitHub {
    owner = "hikhvar";
    repo = "mqtt2prometheus";
    rev = "v${version}";
    hash = "sha256-D5AO6Qsz44ssmRu80PDiRjKSxkOUe4OSm+xtvyGkdUQ=";
  };

  vendorHash = "sha256-5P5J1HwlOFMaGj77k4jU8uJtm0XUIqdPT9abRcvHt2s=";

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  postInstall = ''
    mv $out/bin/cmd $out/bin/mqtt2prometheus
  '';
}
