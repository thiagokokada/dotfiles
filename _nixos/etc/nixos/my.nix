{ lib, ... }:

with lib;
{
  options.my = {
    username = mkOption {
      type = types.str;
    };
  };
}
