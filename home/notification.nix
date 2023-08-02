{ config, pkgs, lib, ... }:
let
  font_size_float = config.personal.font_size;
  font_size = toString font_size_float;
in {
  services.dunst = {
    enable =
      true; # enabled to write config, but service fails when running sway as user
    waylandDisplay = "wayland-1";
    settings = {
      global = {
        offset = "20x20";
        width = "(0, 300)";
        transparency = 0;
        notification_height = 0;
        separator_height = 3;
        padding = 12;
        horizontal_padding = 12;
        frame_width = 3;
        frame_color = "#bdae93";
        separator_color = "frame";
        # idle_threshold = 120;
        font = "Iosevka Lig Medium ${toString (font_size_float + 2)}";
        line_height = 0;
        markup = "full";
        #format = ''<b>%s</b>\n%b'';
        alignment = "right";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = "true";
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "off";
        sticky_history = "yes";
        history_length = 20;
        follow = "keyboard";
      };
      pipestatus = {
        appname = "pipestatus";
        format = "%b";
      };
      urgency_low = {
        background = "#222222";
        foreground = "#888888";
        timeout = 10;
      };
      urgency_normal = {
        background = "#282828";
        foreground = "#ebdbb2";
        timeout = 10;
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = 0;
      };
    };
  };
  systemd.user.services.pipestatus = {
    Unit = {
      Description = "pipestatus";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart =
        "${pkgs.pipestatus.pipestatus-wrapped}/bin/pipestatus-wrapped";
      ExecReload = "kill -SIGUSR2 $MAINPID";
      Restart = "on-failure";
      KillMode = "mixed";
      PrivateTmp = "false";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };
}
