nas:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --flake .#nas --target-host admin@nas

nuc:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --flake .#nuc --target-host admin@nuc --build-host admin@nuc
