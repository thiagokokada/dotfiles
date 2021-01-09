{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nubank/nixpkgs/archive/master.tar.gz";
    }))
  ];

  nixpkgs.config = {
    # For Slack/Zoom.
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    nubank.dart
    nubank.flutter
    nubank.hover
    unstable.slack
    unstable.zoom-us
  ]
  ++ nubank.all-tools;

  # virtualisation = {
  #   # Enable VirtualBox.
  #   virtualbox.host.enable = true;
  # };

  # Added user to groups.
  # users.users.${config.passthru._me.user}.extraGroups = [ "vboxusers" ];
}
