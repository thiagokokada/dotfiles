{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/thiagokokada/i3pyblocks/archive/nix-overlay.tar.gz;
    }))
  ];

  # Configure the virtual console keymap from the xserver keyboard settings.
  console.useXkbConfig = true;

  location = {
    provider = "geoclue2";
  };

  services = {
    # Allow automounting.
    gvfs.enable = true;

    # Load libinput.
    udev.packages = [ pkgs.libinput.out ];

    # For battery status reporting.
    upower.enable = true;

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
        glx-no-stencil = true;
        glx-no-rebind-pixmap = true;
      };
    };

    # Enable gammastep.
    redshift =
    let
      configFile = pkgs.writeText "config.ini" ''
        [general]
        fade=1
        gamma=0.8
      '';
    in {
      enable = true;
      package = pkgs.unstable.gammastep;
      extraOptions = [ "-c ${configFile}" ];
      temperature = {
        day = 5500;
        night = 3700;
      };
      executable = "/bin/gammastep-indicator";
    };

    xserver = {
      enable = true;

      # Enable libinput in X11.
      modules = [ pkgs.xorg.xf86inputlibinput ];

      # Set input config to libinput devices
      inputClassSections = [
        ''
          Identifier "libinput mouse catchall"
          MatchIsPointer "on"
          MatchDevicePath "/dev/input/event*"
          Driver "libinput"
          Option "AccelProfile" "flat"
        ''
        ''
          Identifier "libinput keyboard catchall"
          MatchIsKeyboard "on"
          MatchDevicePath "/dev/input/event*"
          Driver "libinput"
        ''
        ''
          Identifier "libinput touchpad catchall"
          MatchIsTouchpad "on"
          MatchDevicePath "/dev/input/event*"
          Driver "libinput"
          Option "NaturalScrolling" "on"
          Option "Tapping" "on"
          Option "TappingButtonMap" "lmr"
        ''
        ''
          Identifier "libinput touchscreen catchall"
          MatchIsTouchscreen "on"
          MatchDevicePath "/dev/input/event*"
          Driver "libinput"
        ''
        ''
          Identifier "libinput tablet catchall"
          MatchIsTablet "on"
          MatchDevicePath "/dev/input/event*"
          Driver "libinput"
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
          package = pkgs.unstable.i3;
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
            unstable.picom
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
