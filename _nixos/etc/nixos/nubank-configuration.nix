{ config, lib, pkgs, ... }:
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nubank/nixpkgs/archive/master.tar.gz;
    }))
  ];

  nixpkgs.config = {
    # For Slack/Zoom.
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    (yarn.override ({
      nodejs = nodejs-10_x;
    }))
    apacheKafka
    circleci-cli
    docker-compose
    github-cli
    nubank.dart
    nubank.flutter
    nubank.hover
    openfortivpn
    sassc
    unstable.babashka
    unstable.clj-kondo
    unstable.clojure-lsp
    unstable.slack
    unstable.zoom-us
  ]
  ++ nubank.all-tools;

  # virtualisation = {
  #   # Enable VirtualBox.
  #   virtualbox.host.enable = true;
  # };

  # Added user to groups.
  # users.users.thiagoko.extraGroups = [ "vboxusers" ];
}
