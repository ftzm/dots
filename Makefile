nas:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nas --target-host admin@nas --use-remote-sudo

nuc:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nuc --target-host admin@192.168.1.4 --sudo --ask-sudo-password

pi:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa -o SendEnv=-*" nixos-rebuild -v switch --fast --accept-flake-config --flake .#pi --target-host admin@192.168.1.12 --use-remote-sudo

saoiste:
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#saoiste --target-host ftzm@saoiste --use-remote-sudo

switch:
	sudo nixos-rebuild --flake . switch

home:
	home-manager --flake . switch --option fallback true
