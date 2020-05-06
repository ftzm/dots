{ config, lib, ... }:

{
  options.personal.font_size = lib.mkOption {
      type = lib.types.float;
      description = "Base font size.";
  };
  options.personal.alacritty_font_size = lib.mkOption {
      type = lib.types.float;
      description = "Base font size.";
  };
  options.personal.cursor_size = lib.mkOption {
      type = lib.types.int;
      description = "GTK cursor size";
  };
}
