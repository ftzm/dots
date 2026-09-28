{ lib, stdenv, makeWrapper, bash, coreutils, gnugrep, gawk, util-linux, jq, socat, zmx, systemd }:
stdenv.mkDerivation {
  pname = "agentd";
  version = "0.1.0";
  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../Makefile ../bin ../src ../vendor ../hooks
      ../tests/session-tests.c ../tests/fixture-tests.c ../tests/transcript-tests.c
      ../tests/fixtures
    ];
  };
  nativeBuildInputs = [ makeWrapper ];
  buildFlags = [ "build/agentd" "build/agent-replay-filter" ];
  doCheck = true;
  checkTarget = "check-unit";
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin" "$out/libexec/agentd" "$out/share/agentd"
    cp bin/* "$out/libexec/agentd/"
    install -m755 build/agentd build/agent-replay-filter "$out/libexec/agentd/"
    cp hooks/claude.json hooks/codex.json hooks/versions.json "$out/share/agentd/"
    substituteInPlace "$out/libexec/agentd/agent-common" \
      --replace-fail 'agent_data=$agent_root/hooks' "agent_data=$out/share/agentd" \
      --replace-fail 'agent_helpers=$agent_root/build' "agent_helpers=$out/libexec/agentd"
    patchShebangs "$out/libexec/agentd"
    for command in "$out"/libexec/agentd/*; do
      case "$command" in */agent-common|*/agent-replay-filter) continue;; esac
      wrapProgram "$command" --prefix PATH : ${lib.makeBinPath [ bash coreutils gnugrep gawk util-linux jq socat zmx systemd ]}
    done
    for command in agentd agent-new agentctl; do
      ln -s "$out/libexec/agentd/$command" "$out/bin/$command"
    done
    runHook postInstall
  '';
  meta = {
    description = "Persistent coding agents with per-launch hooks and a local state daemon";
    platforms = lib.platforms.linux;
    mainProgram = "agent-new";
  };
}
