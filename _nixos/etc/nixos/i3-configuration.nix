{ pkgs, ... }:

{
  environment = {
    variables = {
      # Export modules to allow PCManFM to use gvfs.
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
    };
  };

  services = {
    # Allow automounting.
    gvfs.enable = true;

    xserver = {
      enable = true;

      # Enable libinput.
      libinput = {
        enable = true;
        naturalScrolling = true;
      };

      # Set input config to libinput devices
      inputClassSections = [
        ''
        Identifier "mouse"
        Driver "libinput"
        MatchIsPointer "on"
        Option "AccelProfile" "flat"
        ''
      ];

      # Use LightDM.
      displayManager = with pkgs; {
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

      # Configure i3-gaps as WM.
      windowManager = {
        default = "i3";
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          # i3 dependencies.
          extraPackages = with pkgs; [
            compton-git
            dex
            dunst
            i3lock
            i3status-rust
            libnotify
            maim
            nitrogen
            rofi
            xdg-user-dirs
            xkblayout-state
            xsecurelock
            xss-lock
          ];
        };
      };

      # Remap Caps Lock to Esc
      xkbOptions = "caps:escape";
    };
  };

  # Enable Qt5 integration.
  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  # Configure special programs (i.e. hardware access).
  programs = {
    # Enable dconf.
    dconf.enable = true;

    # Backlight control.
    light.enable = true;
  };
}
