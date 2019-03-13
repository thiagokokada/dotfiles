{ pkgs, config, ... }:

let
  unstable = import (fetchGit {
    name = "nixos-unstable-2019-03-02";
    url = https://github.com/nixos/nixpkgs/;
    rev = "025c2abd08de80609b961c8c8176a2be390a6d88"; # leiningen 2.9.1
  }) {
    config = config.nixpkgs.config;
  };
in
{
  # For Slack.
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    apacheKafka
    awscli
    clojure
    gnome3.gtk-vnc
    kubectl
    minikube
    nodejs-10_x
    nss
    nssTools
    openssl
    slack
    unstable.leiningen
    vagrant
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

  virtualisation = {
    # Enable Docker.
    docker.enable = true;

    # Enable VirtualBox.
    virtualbox.host.enable = true;
  };

  # Added user to groups.
  users.users.thiagoko.extraGroups = [ "docker" "vboxusers" ];

  services = {
    # Enable CUPS.
    printing = {
      enable = true;
      drivers = [ pkgs.postscript-lexmark ];
    };
  };

  # Enable Emacs daemon, since Spacemacs takes quite a long time to start.
  systemd.user.services.emacs = {
    description = "Emacs: the extensible, self-documenting text editor";
    serviceConfig = {
      Type      = "simple";
      ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment}; exec ${pkgs.emacs}/bin/emacs --fg-daemon'";
      ExecStop  = ''${pkgs.emacs}/bin/emacsclient --eval "(progn (setq kill-emacs-hook 'nil) (kill-emacs))"'';
      Restart   = "on-failure";
    };
    wantedBy = [ "default.target" ];
  };
}
