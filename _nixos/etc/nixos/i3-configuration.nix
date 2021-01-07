{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/thiagokokada/i3pyblocks/archive/nix-overlay.tar.gz;
    }))
  ];

  # Configure the virtual console keymap from the xserver keyboard settings.
  console.useXkbConfig = true;

  # For gammastep;
  location.provider = "geoclue2";

  services = {
    # Allow automounting.
    gvfs.enable = true;

    # For battery status reporting.
    upower.enable = true;

    redshift = {
      enable = true;
      temperature = {
        day = 5500;
        night = 3700;
      };
      gamma = {
        day = 1.0;
        night = 0.8;
      };
      package = pkgs.unstable.gammastep;
      executable = "/bin/gammastep-indicator";
    };

    picom = {
      enable = true;
      experimentalBackends = true;
      fade = true;
      fadeDelta = 2;
      backend = "glx";
      vSync = true;
      settings = {
        unredir-if-possible = true;
        unredir-if-possible-exclude = [ "name *= 'Firefox'" ];
      };
    };

    xserver = {
      enable = true;
      # Recommended for modesetting drivers;
      useGlamor = true;

      # Enable libinput.
      libinput.enable = true;

      # Set input config to libinput devices
      inputClassSections = [
        ''
          Identifier "custom mouse config"
          MatchIsPointer "on"
          Option "AccelProfile" "flat"
        ''
        ''
          Identifier "custom touchpad config"
          MatchIsTouchpad "on"
          Option "NaturalScrolling" "on"
          Option "Tapping" "on"
          Option "TappingButtonMap" "lmr"
        ''
      ];

      # Use LightDM.
      displayManager = with pkgs; {
        defaultSession = "none+i3";

        lightdm = {
          enable = true;
          greeters = {
            gtk = {
              enable = true;
              clock-format = "%a %d/%m %H:%M:%S";
              iconTheme = {
                package = arc-icon-theme;
                name = "Arc";
              };
              indicators = [ "~clock" "~session" "~power" ];
              theme = {
                package = arc-theme;
                name = "Arc-Dark";
              };
            };
          };
        };
      };

      # Configure i3 as WM.
      windowManager = {
        i3 = {
          enable = true;
          package = pkgs.i3;
          # i3 dependencies.
          extraPackages = with pkgs; [
            dex
            dunst
            i3lock
            i3pyblocks
            kbdd
            libnotify
            maim
            mons
            nitrogen
            rofi
            xdg-user-dirs
            xkblayout-state
            xsecurelock
            xsettingsd
            xss-lock
          ];
        };
      };

      # Remap Caps Lock to Esc, and use Super+Space to change layouts
      xkbOptions = "caps:escape,grp:win_space_toggle";
    };
  };

  # Configure special programs (i.e. hardware access).
  programs = {
    # Enable dconf.
    dconf.enable = true;

    # Backlight control.
    light.enable = true;
  };
}
