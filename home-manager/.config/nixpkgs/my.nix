{ lib, ... }:

with lib;

{
  options.my = {
    theme = {
      colors = mkOption {
        type = with types; attrsOf str;
      };
      icon = {
        name = mkOption {
          type = types.str;
        };
        package = mkOption {
          type = with types; nullOr package;
        };
      };
      gtk = {
        name = mkOption {
          type = types.str;
        };
        package = mkOption {
          type = with types; nullOr package;
        };
      };
    };
  };
}
