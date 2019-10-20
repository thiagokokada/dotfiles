{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  networking = {
    # Use Network Manager.
    networkmanager = {
      enable = true;
      dhcp = "internal";
    };
  };

  # Install laptop related packages.
  environment.systemPackages = with pkgs; [
    (callPackage ./pkgs/blueman.nix {})
    iw
  ];

  # Configure special hardware in laptops.
  hardware = {
    # Enable bluetooth.
    bluetooth.enable = true;

    pulseaudio = {
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

  # Enable laptop specific services.
  services = {
    # Trim SSD weekly.
    fstrim = {
      enable = true;
      interval = "weekly";
    };

    # Lock screen when lid is closed.
    logind.lidSwitch = "lock";

    # Enable systemd-resolved
    resolved.enable = true;

    # Enable TLP to reduce power consumption.
    tlp = {
      enable = true;
      extraConfig = ''
        # Radio devices to disable on connect.
        DEVICES_TO_DISABLE_ON_LAN_CONNECT="wifi wwan"
        DEVICES_TO_DISABLE_ON_WIFI_CONNECT="wwan"
        DEVICES_TO_DISABLE_ON_WWAN_CONNECT="wifi"

        # Radio devices to enable on disconnect.
        DEVICES_TO_ENABLE_ON_LAN_DISCONNECT="wifi wwan"
        DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT=""
        DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT=""
      '';
    };
  };
}
