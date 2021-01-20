{ lib, ... }:

with lib;

{
  options.my = {
    theme = {
      colors = mkOption {
        type = with types; attrsOf str;
      };
    };
  };
}
