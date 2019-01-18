{ pkgs, config, ... }:

let
  unstable = import (fetchGit {
    name = "nixos-unstable-2019-01-10";
    url = https://github.com/nixos/nixpkgs/;
    rev = "7c19fba9f61366319428d0a5f4ba2ee1684af5ae"; # leiningen 2.8.3
  }) {
    config = config.nixpkgs.config;
  };
in
{
  # For Slack.
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    (openfortivpn.overrideAttrs (oldAttrs: rec {
      pname = "openfortivpn";
      version = "1.8.1";
      name = "${pname}-${version}";

      src = fetchFromGitHub {
        owner = "adrienverge";
        repo = "${pname}";
        rev = "967d4819475d4f11179960ee50811ec52cd1849c";
        sha256 = "073ywn0m1kxwswlx6avb8yp642h1886kaiiih16j00qf2kyw4if9";
      };
    }))
    awscli
    kubectl
    minikube
    nodejs-10_x
    nss
    nssTools
    openssl
    slack
    unstable.leiningen
  ];

  # Enable Java.
  programs.java = {
    enable = true;
    package = pkgs.jdk8;
  };

  # Enable FortiSSL VPN support in NetworkManager.
  # networking.networkmanager = {
  #   packages = [
  #     pkgs.networkmanager-fortisslvpn
  #   ];
  # };

  # Enable Docker.
  virtualisation.docker.enable = true;

  # Enable VirtualBox.
  virtualisation.virtualbox.host.enable = true;

  users.users.thiagoko.extraGroups = [ "docker" "vboxusers" ];
}
