nas:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nas --target-host admin@nas

nuc:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nuc --target-host admin@nuc # --build-host admin@nuc

switch:
	sudo nixos-rebuild --accept-flake-config --flake . switch
