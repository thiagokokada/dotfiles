{ pkgs, ... }:

{
  # For Slack.
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    (openfortivpn.overrideAttrs (oldAttrs: rec {
      pname = "openfortivpn";
      version = "1.13.3";
      name = "${pname}-${version}";

      src = fetchFromGitHub {
        owner = "adrienverge";
        repo = "${pname}";
        rev = "2d89a16943ed043703b9218da4fc2f8ca02d511c";
        sha256 = "1y3b3zwzig520nyky7xnr0l0zf68i4w698bysyngpkada14d8dv3";
      };
    }))
    apacheKafka
    awscli
    boot
    clj-kondo
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
    minikube
    nss
    nssTools
    python36Packages.jupyter_core
    sassc
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

  # Enable Java anti-aliasing.
  environment.variables._JAVA_OPTIONS = "-Dswing.aatext=TRUE -Dawt.useSystemAAFontSettings=on";

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
  users.users.thiagoko.extraGroups = [ "docker" "vboxusers" "scanner" ];

  hardware.sane.enable = true;

  services = {
    # Enable CUPS.
    printing = {
      enable = true;
      drivers = [ pkgs.postscript-lexmark ];
    };
  };
}
