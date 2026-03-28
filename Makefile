check-not-behind:
	@git fetch origin
	@UPSTREAM=$$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null) && \
	if [ $$(git rev-list HEAD..$$UPSTREAM --count) -ne 0 ]; then \
		echo "Error: Local branch is behind $$UPSTREAM. Pull before rebuilding."; \
		exit 1; \
	fi

nas: check-not-behind
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nas --target-host admin@nas --use-remote-sudo

nuc: check-not-behind
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#nuc --target-host admin@192.168.1.4 --sudo --ask-sudo-password

pi: check-not-behind
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa -o SendEnv=-*" nixos-rebuild -v switch --fast --accept-flake-config --flake .#pi --target-host admin@192.168.1.12 --use-remote-sudo

saoiste: check-not-behind
	sudo NIX_SSHOPTS="-i $$HOME/.ssh/id_rsa" nixos-rebuild -v switch --fast --accept-flake-config --flake .#saoiste --target-host ftzm@saoiste --use-remote-sudo

switch: check-not-behind
	sudo nixos-rebuild --flake . switch

home: check-not-behind
	home-manager --flake . switch --option fallback true
