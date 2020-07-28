{ config, lib, pkgs, ... }:
let
  flutterDir = "$HOME/sdk-flutter";
  patchFlutter = pkgs.writeShellScriptBin "patch-flutter" ''
    isScript() {
        local fn="$1"
        local fd
        local magic
        exec {fd}< "$fn"
        read -r -n 2 -u "$fd" magic
        exec {fd}<&-
        if [[ "$magic" =~ \#! ]]; then return 0; else return 1; fi
    }

    stopNest() { true; }

    source ${<nixpkgs/pkgs/build-support/setup-hooks/patch-shebangs.sh>}
    patchShebangs --build ${flutterDir}/bin/
    find ${flutterDir}/bin/ -executable -type f -exec ${pkgs.patchelf}/bin/patchelf --set-interpreter ${pkgs.glibc}/lib/ld-linux-x86-64.so.2 {} \;
  '';
  hover-flutter = (import (fetchTarball https://github.com/ericdallo/nixpkgs/archive/hover-flutter.tar.gz) {});
in {
  nixpkgs.config = {
    # For Slack/Zoom.
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    (leiningen.override ({
      jdk = pkgs.jdk11;
    }))
    apacheKafka
    awscli
    # TODO: Return to use it when graalVM stops failing to build
    # clj-kondo
    clojure
    docker-compose
    gitAndTools.hub
    go
    hover-flutter.hover
    jetbrains.idea-community
    joker
    jupyter
    kubectl
    minikube
    nss
    nssTools
    patchFlutter
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
