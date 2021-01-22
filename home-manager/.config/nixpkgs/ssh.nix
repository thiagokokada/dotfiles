{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "*" = {
        serverAliveInterval = 300;
        serverAliveCountMax = 2;
        user = config.home.username;
        extraOptions = {
          AddKeysToAgent = "yes";
        };
      };
    };
  };
}
