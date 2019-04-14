{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  networking = {
    # Use wpa_supplicant.
    # wireless.enable = true;

    # Use Network Manager.
    networkmanager = {
      enable = true;
      dhcp = "internal";
      dns = "dnsmasq";
    };
  };

  # Install laptop related packages.
  environment.systemPackages = with pkgs; [
    blueman
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

    # Enable TLP to reduce power consumption.
    tlp = {
      enable = true;
      extraConfig = ''
        # Enable powersave governor
        CPU_SCALING_GOVERNOR_ON_AC=powersave
        CPU_SCALING_GOVERNOR_ON_BAT=powersave
      '';
    };
  };
}
