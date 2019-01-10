{ pkgs, ... }:

{
  # For Slack.
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    (leiningen.overrideAttrs(oldAttrs: rec {
      pname = "leiningen";
      version = "2.7.1";
      name = "${pname}-${version}";

      src = fetchurl {
        url = "https://raw.github.com/technomancy/leiningen/${version}/bin/lein-pkg";
        sha256 = "0rmshl4xchf3blwvar4q9dpxm9xznn3yzas4vwxqiq3yhapgqkn0";
      };

      jarsrc = fetchurl {
        url = "https://github.com/technomancy/leiningen/releases/download/${version}/${name}-standalone.zip";
        sha256 = "0ivwb1qlxs1hyical0fjgavm9wfkw3f10sk67p5g2p5lpf4pxp1d";
      };
    }))
    aws
    kubectl
    minikube
    nodejs-10_x
    openfortivpn
    openssl
    slack
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
