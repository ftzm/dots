{ lib, emacsPackages }:
emacsPackages.trivialBuild {
  pname = "agentd";
  version = "0.1.0";
  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../agentd.el ../agentd-client.el ../agentd-status.el
      ../agentd-terminal.el ../agentd-attention.el ../agentd-completion.el
      ../agentd-launch.el ../agentd-session.el ../agentd-perspective.el
      ../agentd-recovery.el ../agentd-overview.el
    ];
  };
  packageRequires = [ emacsPackages.ghostel emacsPackages.marginalia ];
  # Also supply an ELPA archive for an existing, non-Nix-managed Emacs.
  postInstall = ''
    mkdir -p archive/agentd-0.1.0
    cp agentd*.el archive/agentd-0.1.0/
    cat > archive/agentd-0.1.0/agentd-pkg.el <<'EOF'
    ;; -*- no-byte-compile: t; lexical-binding: nil -*-
    (define-package "agentd" "0.1.0" "Persistent coding agents in Ghostel"
      '((emacs "30.1") (ghostel "0.52.0") (marginalia "2.0")))
    EOF
    tar -C archive -cf "$out/share/emacs/agentd-0.1.0.tar" agentd-0.1.0
  '';
  meta.description = "Ghostel UI and attention tracking for persistent coding agents";
}
