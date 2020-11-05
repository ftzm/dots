{ config, lib, ... }:

let
  pkgs = import ./pkgs.nix;
  home_manager_repo = (import ../nix/sources.nix).home-manager.outPath;
  conditional_imports =
    lib.attrByPath [ (builtins.readFile /etc/nixos/model) ] [ ] {
      "ThinkPad T480s" = [ ./personal.nix ];
      "ThinkPad X1 Extreme 2nd" = [ ./unity.nix ];
    };
in {
  _module.args.pkgs = lib.mkForce pkgs;
  nixpkgs.config = { firefox = { enableTridactylNative = true; }; };
  imports = [
    ./personal-options.nix
    ./packages.nix
    ./services.nix
    ./shell.nix
    ./development.nix
    ./mail.nix
    ./xorg.nix
    ./pipestatus.nix
  ] ++ conditional_imports;
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.home-manager.path = "${home_manager_repo}";

  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Lig normal ${toString (config.personal.font_size + 1)}";
      default-bg = "#282828";
      statusbar-bg = "#282828";
      statusbar-fg = "#ebdbb2";
    };
  };
  programs.firefox = {
    enable = true;
    profiles.main = {
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "browser.startup.homepage" = "www.google.com";
      };
      userChrome = ''


          /* Hide tabs */

          #TabsToolbar {
            visibility: collapse !important;
            margin-bottom: 21px !important;
          }

          /* Hide sidebar header */

          #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
            visibility: collapse !important;
          }
          .sidebar-splitter { display: none;}

          /* Autohide sidebar */

:root {
  --sidebar-hover-width: 8px;
  --sidebar-visible-width: 320px;
}
#sidebar-box {
  position: relative !important;
  transition: all 200ms !important;
  min-width: var(--sidebar-hover-width) !important;
  max-width: var(--sidebar-hover-width) !important;
  opacity: 0 !important;
  transition: all 250ms cubic-bezier(0.075, 0.820, 0.165, 1.000);
}
#sidebar-box:hover {
  transition: all 200ms !important;
  min-width: var(--sidebar-visible-width) !important;
  max-width: var(--sidebar-visible-width) !important;
  margin-right: calc((var(--sidebar-visible-width) - var(--sidebar-hover-width)) * -1) !important;
  z-index:1;
  opacity: 1 !important;
  transition: all 250ms cubic-bezier(0.075, 0.820, 0.165, 1.000);
}

#sidebar {
  opacity: 0 !important;
}

#sidebar:hover {
  opacity: 1 !important;
}


        '';
    };
  };
}
