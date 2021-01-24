{ lib, ... }:

with lib;
let
  themeType = types.submodule {
    options = {
      package = mkOption { type = with types; nullOr package; };

      name = mkOption { type = types.str; };
    };
  };
in {
  options.my = {
    dotfilesDir = mkOption { type = types.path; };

    deviceType = mkOption {
      type = types.enum [ "desktop" "notebook" ];
      default = "desktop";
    };

    fonts = { gui = mkOption { type = types.nullOr themeType; }; };

    terminal = mkOption { type = types.str; };

    theme = { colors = mkOption { type = with types; attrsOf str; }; };
  };
}
