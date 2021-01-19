{ lib, ... }:

with lib;

{
  options.my = {
    theme = {
      icon = {
        name = mkOption {
          type = types.str;
        };
        package = mkOption {
          type = types.package;
        };
      };
      colors = mkOption {
        type = with types; attrsOf str;
      };
    };
  };
}
