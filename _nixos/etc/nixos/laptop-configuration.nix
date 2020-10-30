{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  networking = {
    # Use Network Manager.
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
      dispatcherScripts = [
        {
          source = pkgs.writeScript "disable-wifi-on-ethernet" ''
            #!${pkgs.bash}/bin/bash

            interface="$1"
            iface_mode="$2"
            iface_type=$(nmcli dev | grep "$interface" | tr -s ' ' | cut -d' ' -f2)
            iface_state=$(nmcli dev | grep "$interface" | tr -s ' ' | cut -d' ' -f3)

            enable_wifi() {
              nmcli radio wifi on
            }

            disable_wifi() {
              nmcli radio wifi off
            }

            if [ "$iface_type" = "ethernet" ] && [ "$iface_mode" = "down" ]; then
              enable_wifi
            elif [ "$iface_type" = "ethernet" ] && [ "$iface_mode" = "up"  ] && [ "$iface_state" = "connected" ]; then
              disable_wifi
            fi
          '';
        }
      ];
    };
    wireless = {
      enable = false;
      iwd.enable = true;
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
