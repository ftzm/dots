{ config, lib, ... }:

{
  options.personal.font_size = lib.mkOption {
      type = lib.types.float;
      description = "Base font size.";
  };
}
