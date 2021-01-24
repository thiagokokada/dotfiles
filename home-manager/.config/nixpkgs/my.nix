{ pkgs, lib, ... }:

with lib;
let
  themeType = types.submodule {
    options = {
      package = mkOption {
        type = with types; nullOr package;
        description = "GTK theme";
      };

      name = mkOption {
        type = types.str;
        description = "GTK theme name";
      };
    };
  };
in {
  options.my = {
    dotfilesDir = mkOption {
      type = types.path;
      default = ../../..;
    };

    deviceType = mkOption {
      type = types.enum [ "desktop" "notebook" ];
      default = "desktop";
    };

    mountPoints = mkOption {
      type = with types; nullOr (listOf str);
      default = null;
      example = [ "/" "/mnt/backup" ];
    };

    fonts = {
      gui = mkOption {
        type = types.nullOr themeType;
        description = "GUI main font";
      };
    };

    terminal = mkOption {
      type = types.str;
      default = "${pkgs.kitty}/bin/kitty";
    };

    theme = {
      colors = mkOption {
        type = with types; attrsOf str;
        description = "Base16 colors";
      };
    };
  };
}
