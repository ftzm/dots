self: super:

with super.lib;

let
  # Recursively constructs an attrset of a given folder, recursing on
  # directories, value of attrs is the filetype
  getDir = dir:
    mapAttrs
    (file: type: if type == "directory" then getDir "${dir}/${file}" else type)
    (builtins.readDir dir);

  # Collects all files of a directory as a list of strings of paths
  files = dir:
    collect isString
    (mapAttrsRecursive (path: type: concatStringsSep "/" path) (getDir dir));

  # Filters out directories that don't end with .nix or are this file
  validFiles = dir:
    filter (file: hasSuffix ".nix" file && file != "default.nix") (files dir);

  # Create an attrset singleton with the name and package of a file.
  overlayAttr = file: {
    name = removeSuffix ".nix" file;
    value = super.callPackage (./. + "/${file}") { };
  };

  mkOverlays = dir: listToAttrs (map overlayAttr (validFiles dir));

  overlays = mkOverlays ./.;

in overlays
