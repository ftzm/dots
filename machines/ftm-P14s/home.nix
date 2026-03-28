{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
let
  # Wrap xdg-desktop-portal-wlr with nixGLIntel so it can find Mesa/DRI
  # drivers on non-NixOS. Without this, the portal segfaults because the
  # Nix-built binary looks for drivers at /run/opengl-driver/ (NixOS-only).
  xdg-desktop-portal-wlr-gl = pkgs.stdenv.mkDerivation {
    name = "xdg-desktop-portal-wlr-gl";
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/libexec
      cat > $out/libexec/xdg-desktop-portal-wlr <<EOF
#!/bin/sh
exec ${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr "\$@"
EOF
      chmod +x $out/libexec/xdg-desktop-portal-wlr

      cp -r --no-preserve=mode ${pkgs.xdg-desktop-portal-wlr}/share $out/share

      substituteInPlace $out/share/systemd/user/xdg-desktop-portal-wlr.service \
        --replace-fail "${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr" \
          "$out/libexec/xdg-desktop-portal-wlr"
      substituteInPlace $out/share/dbus-1/services/org.freedesktop.impl.portal.desktop.wlr.service \
        --replace-fail "${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr" \
          "$out/libexec/xdg-desktop-portal-wlr"
    '';
  };
in
{
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      inputs.nixgl.overlay
    ];
  };

  imports = [
    ../../role/iosevka-home.nix
    ../../role/emacs-home.nix
    ../../role/sway-home.nix
    ../../role/battery-home.nix
  ];

  home.username = "ftzm";
  home.homeDirectory = "/home/ftzm";

  home.packages = with pkgs; [
    firefox
    gnumake # to run makefiles
    nil # nix language server
    alejandra # nix formatter
    awscli # aws cli tool
    gh # github cli tool
    tofi
    jetbrains.datagrip
    spotify
    gettext
    scalafix
    shfmt
    shellcheck
    ulid
    insomnia
    wdisplays
    htop
    xdg-desktop-portal

    nixgl.nixGLIntel
    nixgl.nixVulkanIntel

    # core
    php83
    php83Packages.composer
    php83Packages.psalm
    php83Extensions.curl
    php83Extensions.intl
    php83Extensions.xml
    php83Extensions.dom
    php83Extensions.mbstring
    intelephense

    # ai
    (python3.withPackages
      (ps:
        with ps; [
          numpy
          pandas
          matplotlib
          seaborn
        ]))
    uv
    aider-chat
    jetbrains-mono
    nodejs
    yarn
    crowdin-cli
    claude-code
    ripgrep
    fd
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/ftzm/etc/profile.d/hm-session-vars.sh
  #
  home.sessionPath = ["$HOME/dots/bin"];

  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # automatically rebuild font cache
  fonts.fontconfig.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableBashIntegration = true;
  };

  programs.git = {
    enable = true;
    ignores = [
      # Emacs
      "**/*~"
      "**/*#"
      # Python
      ".mypy_cache"
      # direnv
      ".direnv"
      # Haskell
      "*.hi"
      "*.o"
      # Nix
      "result"
    ];
    settings.user.email = "mft@famly.co";
    settings.user.name = "ftzm";
    settings = {
      credential.helper = "!gh auth git-credential";
      github = {
        user = "ftzm";
      };
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
      tag.sort = "version:refname";
      push = {
        default = "simple";
        autoSetupRemote = "true";
        followTags = "true";
      };
      fetch = {
        prune = "true";
        pruneTags = "true";
        all = "true";
      };
      commit = {
        verbose = "true";
      };
      rerere = {
        enable = "true";
        autoupdate = "true";
      };
      rebase = {
        autoSquash = "true";
        autoStash = "true";
        updateRefs = "true";
      };
      merge = {
        conflictstyle = "zdiff3";
      };
      core = {
        fsmonitor = "true";
        untrackedCache = "true";
      };
    };
  };

  programs.mergiraf.enable = true;
  programs.mergiraf.enableGitIntegration = true;

  home.activation = {
    myActivationAction = inputs.home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
      cd /home/ftzm/dots
      ${pkgs.stow}/bin/stow -t $HOME --no-folding dotfiles
    '';
  };

  programs.bash = {
    enable = true;
    profileExtra = let
      jdk = "/home/ftzm/.cache/coursier/arc/https/github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.7%252B6/OpenJDK21U-jdk_x64_linux_hotspot_21.0.7_6.tar.gz/jdk-21.0.7+6";
    in ''
      # >>> JVM installed by coursier >>>
      export JAVA_HOME="${jdk}"
      export PATH="$PATH:${jdk}/bin"
      # <<< JVM installed by coursier <<<

      # >>> coursier install directory >>>
      export PATH="$PATH:/home/ftzm/.local/share/coursier/bin"
      # <<< coursier install directory <<<
    '';
  };
  programs.atuin = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      # auto_sync = true;
      sync_frequency = "5m";
      # sync_address = "http://wg-nuc:8889";
      search_mode = "fuzzy";
      sync = {
        records = true;
      };
    };
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      add_newline = false;
      format = lib.concatStrings [
        "$directory"
        "$cmd_duration"
        "$line_break"
        "$character"
      ];
      scan_timeout = 10;
      character = {
        success_symbol = "[\\$](green)";
        error_symbol = "[\\$](red)";
      };
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = [
      xdg-desktop-portal-wlr-gl
      pkgs.xdg-desktop-portal-gtk
    ];
    config = {
      sway = {
        default = ["gtk"];
        "org.freedesktop.impl.portal.Screenshot" = ["wlr"];
        "org.freedesktop.impl.portal.ScreenCast" = ["wlr"];
      };
    };
  };
  # Auto-start the wlr portal with the sway session so we don't rely on
  # D-Bus activation (D-Bus can't find the nix service files).
  systemd.user.services.xdg-desktop-portal-wlr-autostart = {
    Unit = {
      Description = "Auto-start xdg-desktop-portal-wlr";
      After = ["graphical-session.target"];
      Wants = ["xdg-desktop-portal-wlr.service"];
    };
    Install = {
      WantedBy = ["sway-session.target"];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.coreutils}/bin/true";
    };
  };
  # P14s-specific sway overrides
  wayland.windowManager.sway = {
    systemd.enable = true;
    config = {
      startup = [
        {
          command = "swaymsg output eDP-1 scale 2";
          always = true;
        }
        {command = "ci-watch";}
      ];
      keybindings = let
        inherit (config.wayland.windowManager.sway.config) modifier;
      in
        lib.mkOptionDefault {
          "${modifier}+space" = "exec 'tofi-run --width 40% --height 30% | xargs swaymsg exec --'";
        };
      modes.system = let
        md = "swaymsg mode default;";
      in {
        "l" = ''exec "${md}swaylock -i ~/dots/stacks.jpg"'';
      };
    };
  };

  programs.foot.settings.main.font = "iosevka ftzm:medium:size=11";

  services.mako = {
    enable = true;
    settings = {
      font = "Iosevka Ftzm 11";
      background-color = "#282828";
      border-color = "#fabd2f";
      border-size = 3;
      text-color = "#ebdbb2";
      margin = "15";
      padding = "10";
};
  };

  # workaround for systemd units not being loaded by ubuntu
  home.activation.linkSystemd = let
    inherit (lib) hm;
  in
    hm.dag.entryBefore ["reloadSystemd"] ''
      find $HOME/.config/systemd/user/ \
        -type l \
        -exec bash -c "readlink {} | grep -q $HOME/.nix-profile/share/systemd/user/" \; \
        -delete

      find $HOME/.nix-profile/share/systemd/user/ \
        -type f -o -type l \
        -exec ln -s {} $HOME/.config/systemd/user/ \;
    '';

  # programs.vscode.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.vscode = {
    enable = true;
    # package = pkgs.vscode.fhs;
  };
  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.
}
