{ pkgs, ... }:

{
  nixpkgs.config = {
    # For Slack/Zoom.
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    (leiningen.override ({
      jdk = pkgs.jdk8;
    }))
    apacheKafka
    awscli
    # TODO: Return to use it when graalVM stops failing to build
    # clj-kondo
    clojure
    docker-compose
    gitAndTools.hub
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

  # Hack for problematic Dell Network Adapter
  systemd.services.r8152-fix = {
    description = "Restart RTL-8152 module on resume from suspend";
    after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
    wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
    serviceConfig = {
      ExecStartPre = ''${pkgs.kmod}/bin/modprobe -r r8152'';
      ExecStart = ''${pkgs.kmod}/bin/modprobe r8152'';
    };
  };

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
