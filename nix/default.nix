{ pkgs ? (import <nixpkgs> { config = { allowBroken = true; }; }), ... }:

pkgs.runCommand "bootstrap-system" {
  preferLocalBuild = true;
  allowSubstitutes = false;
  shellHookOnly = true;
  shellHook = ''
    echo
    # This should be run as root.
    NIX_USER=matt

    # Get the model name and place at /etc/nixos/model so that configuration.nix
    # can select the appropriate hardware tweaks.
    echo "* Recording hardware model"
    ${pkgs.dmidecode}/bin/dmidecode -t1 |
    grep Version |
    cut -d" " -f2- |
    tr -d '\n' > /etc/nixos/model
    echo "Done"
    echo

    echo "* Setting state version"
    # Get state version from inital generation
    .${toString ./.}/state_version.sh
    echo

    echo "* Running initial nixos generation"
    # Update configuration. On first run, this will create the non-root user,
    # after which user-land setup can take place.
    nixos-rebuild switch \
      -I nixos-config=${toString ./.}/src/configuration/configuration.nix
    echo

    echo "* Setting user password"
    # Set user password
    PASSWORD_STATUS=$(passwd --status $NIX_USER | cut -d" " -f2)
    if [ "$PASSWORD_STATUS" = "NP" ]; then
      passwd $NIX_USER
    else
      echo "Password already set, skipping creation."
    fi
    echo

    echo "* Correcting repo location"
    # Move to user home if applicable
    LOCATION=$(git rev-parse --show-toplevel)
    CORRECT_LOCATION="/home/$NIX_USER/.dots"
    if ! [[ "$LOCATION" == "$CORRECT_LOCATION" ]]; then
      mv $LOCATION $CORRECT_LOCATION
      chown -R matt:users $CORRECT_LOCATION
    fi
    echo

    echo "* Assuming $NIX_USER"
    sudo -i -u $NIX_USER bash << EOF
    cd $CORRECT_LOCATION
    echo

    echo "* Installing user-land configuration"
    ./install.sh -f MODULES -y
    echo

    echo "* Bootstrapping home manager"
    (cd nix; ./bootstrap-home-manager.sh)
    EOF
    echo

    echo "Installation complete."

    exit 0
  '';
} ''
  echo This derivation is not buildable, instead run it using nix-shell.
  exit 1
''
