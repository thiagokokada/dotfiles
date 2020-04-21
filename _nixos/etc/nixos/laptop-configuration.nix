{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  networking = {
    # Use Network Manager.
    networkmanager = {
      enable = true;
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
      extraConfig = ''
        # Switch between headset and headphone mode (e.g. for calls and music) automatically
        load-module module-bluetooth-policy auto_switch=2
        # Echo cancellation and noise cleanup of mic
        load-module module-echo-cancel aec_method=webrtc
      '';
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

    # Enable TLP to reduce power consumption.
    tlp = {
      enable = true;
      extraConfig = ''
        CPU_SCALING_GOVERNOR_ON_AC=powersave
        CPU_SCALING_GOVERNOR_ON_BAT=powersave

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
