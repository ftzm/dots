{
  config,
  pkgs,
  inputs,
  lib,
  ...
}: {
  nixpkgs.config.allowUnfree = true;

  imports = [
    ../../role/iosevka-home.nix
    ../../role/emacs-home.nix
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
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

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
    userEmail = "mft@famly.co";
    userName = "ftzm";
    extraConfig = {
      status = {
        showUntrackedFiles = "all"; # allows magit to show dir contents
      };
    };
  };

  home.activation = {
    myActivationAction = inputs.home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
      cd /home/ftzm/dots
      ${pkgs.stow}/bin/stow -t $HOME --no-folding dotfiles
    '';
  };

  programs.bash = {
    enable = true;
    profileExtra = ''
      # >>> JVM installed by coursier >>>
      export JAVA_HOME="/home/ftzm/.cache/coursier/arc/https/github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.5%252B11/OpenJDK21U-jdk_x64_linux_hotspot_21.0.5_11.tar.gz/jdk-21.0.5+11"
      export PATH="$PATH:/home/ftzm/.cache/coursier/arc/https/github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.5%252B11/OpenJDK21U-jdk_x64_linux_hotspot_21.0.5_11.tar.gz/jdk-21.0.5+11/bin"
      # <<< JVM installed by coursier <<<

      # >>> coursier install directory >>>
      export PATH="$PATH:/home/ftzm/.local/share/coursier/bin"
      # <<< coursier install directory <<<

      #workaround for running sway on mft-P14s
      export WLR_RENDERER=vulkan
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

  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;
    wrapperFeatures.gtk = true;
    config = {
      bars = [
        {
          command = "waybar";
          mode = "invisible";
          # hiddenState = "hide";
          # extraConfig = ''
          #   modifier Mod4
          # '';
        }
      ];
      startup = [
        {
          command = "swaymsg output eDP-1 scale 2";
          always = true;
        }
      ];
      modifier = "Mod4";
      terminal = "foot";
      gaps = {
        smartBorders = "on";
        # smartGaps = true;
        # inner = 10;
      };
      window = {
        titlebar = false;
        border = 3;
      };
      keybindings = let
        inherit (config.wayland.windowManager.sway.config) modifier;
      in
        lib.mkOptionDefault {
          "${modifier}+Shift+r" = "reload";
          #"${modifier}+Shift+c" = "kill";
          "${modifier}+space" = "exec tofi-drun --drun-launch=true -c ~/.config/tofi/dmenu";
          # "${modifier}+Shift+b" = "exec splitv";
          "${modifier}+v" = "exec volumectl -p -u up";
          "${modifier}+Shift+v" = "exec volumectl -p -u down";
          "${modifier}+F1" = "exec volumectl toggle-mute";
          "${modifier}+p" = "exec mpc toggle";
          # "${modifier}+Shift+z" = "exec fzf_key.sh";
          "${modifier}+s" = ''mode "system"'';
          # "${modifier}+o" = ''mode "org"'';
          "${modifier}+f" = "exec dired.sh";
          "${modifier}+Shift+z" = "exec clip_key";
          "${modifier}+m" = "exec '[ \"$(swaymsg -t get_bar_config bar-0 | jq -r \".mode\")\" = \"dock\" ] && swaymsg bar mode invisible || swaymsg bar mode dock'";
        };
      modes.system = let
        md = "swaymsg mode default;";
        brightness = let
          toKey = n: {
            name = toString n;
            value = "exec lightctl set ${toString (n + 1)}0%; mode default";
          };
        in
          builtins.listToAttrs (map toKey (lib.lists.range 0 9));
      in
        {
          #"l" = ''exec "${md}${swaylockCmd}"'';
          "l" = ''exec "${md}swaylock -i ~/dots/stacks.jpg"'';
          # "d" = ''exec "${md}echo '?' > /tmp/statuspipe.fifo"'';
          "Escape" = "mode default";
          "Return" = "mode default";
        }
        // brightness;
      # modes.org = let
      #   md = "swaymsg mode default;";
      # in {
      #   "t" = ''exec "${md}capture.sh t"'';
      #   "w" = ''exec "${md}capture.sh w"'';
      #   "Escape" = "mode default";
      #   "Return" = "mode default";
      # };
      colors = {
        focused = {
          background = "#5f676a";
          border = "#000000";
          childBorder = "#458588";
          indicator = "#484e50";
          text = "#ffffff";
        };
        unfocused = {
          background = "#5f676a";
          border = "#000000";
          childBorder = "#282828";
          indicator = "#484e50";
          text = "#ffffff";
        };
      };
    };
    extraConfig = ''
      # for_window [title="capture"] floating enable, resize set 660 300
      # for_window [title="capture"] border pixel 1
      # for_window [app_id="foot-launcher"] floating enable, resize set 400 300
      # for_window [app_id="foot-launcher"] border pixel 1

      # default_border pixel 0

      # smart_gaps on
      # gaps inner 1
      # gaps outer -1


      # hide_edge_borders smart
      seat seat0 xcursor_theme Vanilla-DMZ 64

      set $TBALL1 1149:32792:Kensington_Expert_Wireless_TB_Mouse
      input $TBALL1 {
        scroll_method on_button_down
        scroll_button 275
        scroll_factor 0.3
      }

      set $TBALL2 1149:32793:ExpertBT5.0_Mouse
      input $TBALL2 {
        scroll_method on_button_down
        scroll_button 275
        scroll_factor 0.3
      }

      # exec systemctl --user import-environment XDG_SESSION_TYPE XDG_CURRENT_DESKTOP
      # exec dbus-update-activation-environment WAYLAND_DISPLAY
    '';
  };

  programs.waybar = {
    enable = true;
    settings = [
      {
        "bar_id" = "bar-0";
        "ipc" = true;
        "modules-left" = [
          "sway/workspaces"
          "sway/mode"
          "sway/scratchpad"
        ];
        "modules-center" = ["sway/window"];
        "modules-right" = [
          "mpd"
          "idle_inhibitor"
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "keyboard-state"
          "sway/language"
          "battery"
          "clock"
          # "tray"
        ];
        "position" = "bottom";
      }
    ];
  };

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "iosevka ftzm:medium:size=11";
        # letter-spacing = "-0.2";
      };
      cursor = {color = "282828 ebdbb2";};
      colors = {
        background = "282828";
        foreground = "ebdbb2";
        regular0 = "282828";
        regular1 = "cc241d";
        regular2 = "98971a";
        regular3 = "d79921";
        regular4 = "458588";
        regular5 = "b16286";
        regular6 = "689d6a";
        regular7 = "a89984";
        bright0 = "928374";
        bright1 = "fb4934";
        bright2 = "b8bb26";
        bright3 = "fabd2f";
        bright4 = "83a598";
        bright5 = "d3869b";
        bright6 = "8ec07c";
        bright7 = "ebdbb2";
      };
    };
  };

  services.avizo = {
    enable = true;
    settings = {
      default = {
        time = 1.0;
        y-offset = 0.5;
        fade-in = 0.1;
        fade-out = 0.2;
      };
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

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.
}
