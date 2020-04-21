{ pkgs, ... }:

{
  environment = {
    variables = {
      # Export modules to allow PCManFM to use gvfs.
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
    };
  };

  # Configure the virtual console keymap from the xserver keyboard settings.
  console.useXkbConfig = true;

  services = {
    # Allow automounting.
    gvfs.enable = true;

    # Load libinput.
    udev.packages = [ pkgs.libinput.out ];

    # For battery status reporting.
    upower.enable = true;

    xserver = {
      enable = true;

      # Enable libinput in X11.
      modules = [ pkgs.xorg.xf86inputlibinput ];

      # Set input config to libinput devices
      inputClassSections = [
        ''
          Identifier "mouse"
          Driver "libinput"
          MatchIsPointer "on"
          Option "AccelProfile" "flat"
        ''
        ''
          Identifier "touchpad"
          Driver "libinput"
          MatchIsTouchpad "on"
          Option "NaturalScrolling" "true"
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

      # Configure i3-gaps as WM.
      windowManager = {
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
            kbdd
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

      # Remap Caps Lock to Esc, and use Super+Space to change layouts
      xkbOptions = "caps:escape,grp:win_space_toggle";
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

    # SpaceFM.
    spacefm.enable = true;
  };
}
