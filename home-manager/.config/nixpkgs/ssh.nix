{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    compression = true;
    serverAliveInterval = 300;
    serverAliveCountMax = 2;
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };
}
