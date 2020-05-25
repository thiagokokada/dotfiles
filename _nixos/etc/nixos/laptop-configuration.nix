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
          source = pkgs.writeText "disable-wifi-on-ethernet" ''
            #!${pkgs.bash}/bin/bash
            wired_interfaces="en.*|eth.*"
            if [[ "$1" =~ $wired_interfaces ]]; then
              case "$2" in
                up)
                  nmcli radio wifi off
                  ;;
                down)
                  nmcli radio wifi on
                  ;;
              esac
            fi
          '';
          type = "basic";
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
