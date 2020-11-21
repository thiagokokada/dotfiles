{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  networking = {
    # Use Network Manager.
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };

  # Install laptop related packages.
  environment.systemPackages = with pkgs; [
    iw
  ];

  # Configure special hardware in laptops.
  hardware = {
    # Enable bluetooth.
    bluetooth.enable = true;

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      # Enable extra bluetooth codecs.
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };

    # Extra OpenGL options.
    opengl = {
      extraPackages = with pkgs; [
        libvdpau-va-gl
        vaapiIntel
        vaapiVdpau
      ];
    };
  };

  # Enable programs that need special configuration.
  programs = {
    # Enable NetworkManager applet.
    nm-applet.enable = true;
  };

  # Make nm-applet restart in case of failure
  systemd.user.services.nm-applet = {
    serviceConfig = {
      RestartSec = 3;
      Restart = "on-failure";
    };
  };

  # Enable laptop specific services.
  services = {
    # Enable Blueman to manage Bluetooth.
    blueman.enable = true;

    # Suspend when power key is pressed
    logind = {
      lidSwitch = "suspend-then-hibernate";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "ignore";

      extraConfig = ''
        HandlePowerKey=suspend-then-hibernate
      '';
    };
  };
}
