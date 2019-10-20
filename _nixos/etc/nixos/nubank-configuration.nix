{ pkgs, ... }:

{
  # For Slack.
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    apacheKafka
    awscli
    clojure
    docker-compose
    elixir
    gitAndTools.hub
    graalvm8
    graphviz
    insomnia
    jetbrains.idea-community
    joker
    jupyter
    kubectl
    leiningen
    lumo
    minikube
    nss
    nssTools
    opam
    python36Packages.jupyter_core
    slack
    tigervnc
    vagrant
    zoom-us
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
}
