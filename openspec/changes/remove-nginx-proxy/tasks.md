## 1. Reconfigure nginx

- [ ] 1.1 Change WebDAV nginx to listen on 127.0.0.1 only (machines/nuc/default.nix)
- [ ] 1.2 Remove all nginx virtualHosts except WebDAV backend (machines/nuc/default.nix)
- [ ] 1.3 Remove nginx WG catch-all proxy (machines/nuc/default.nix)
- [ ] 1.4 Remove `security.acme` config (machines/nuc/default.nix)
- [ ] 1.5 Verify nuc config evaluates cleanly
- [ ] 1.6 Verify WebDAV still works through Traefik → nginx
