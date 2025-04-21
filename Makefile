nas:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nas --target-host admin@nas --use-remote-sudo

nuc:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nuc --target-host admin@nuc --use-remote-sudo

pi:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#pi --target-host admin@pi --use-remote-sudo

saoiste:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#saoiste --target-host ftzm@saoiste --use-remote-sudo

switch:
	sudo nixos-rebuild --verbose --flake . switch
