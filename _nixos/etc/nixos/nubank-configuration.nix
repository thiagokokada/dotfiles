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
    (leiningen.override ({
      jdk = pkgs.jdk8;
    }))
    (yarn.override ({
      nodejs = nodejs-10_x;
    }))
    apacheKafka
    awscli
    # TODO: Return to use it when graalVM stops failing to build
    # clj-kondo
    clojure
    docker-compose
    nubank.flutter
    nubank.dart
    nubank.hover
    gitAndTools.hub
    go
    jetbrains.idea-community
    joker
    jupyter
    kubectl
    minikube
    nss
    nssTools
    python37Packages.jupyter_core
    sassc
    unstable.circleci-cli
    unstable.clojure-lsp
    unstable.lumo
    unstable.openfortivpn
    unstable.slack
    unstable.zoom-us
    vagrant
  ];

  # Enable Java.
  programs.java = {
    enable = true;
    package = pkgs.jdk11;
  };

  # Enable Java anti-aliasing.
  environment.variables._JAVA_OPTIONS = "-Dswing.aatext=TRUE -Dawt.useSystemAAFontSettings=on";

  virtualisation = {
    # Enable Docker.
    docker.enable = true;

    # Enable VirtualBox.
    virtualbox.host.enable = true;
  };

  # Added user to groups.
  users.users.thiagoko.extraGroups = [ "docker" "vboxusers" "scanner" ];

  services = {
    # Enable CUPS.
    printing = {
      enable = true;
      drivers = [ pkgs.postscript-lexmark ];
    };
  };
}
