{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/thiagokokada/i3pyblocks/archive/nix-overlay.tar.gz;
    }))
  ];

  environment = {
    systemPackages = with pkgs; [
      arc-icon-theme
      arc-theme
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    etc."xdg/gtk-2.0/gtkrc" = {
      text = ''
        gtk-icon-theme-name = "Adwaita"
        gtk-theme-name = "Arc-Dark"
        gtk-cursor-theme-name = "Adwaita"
        gtk-fallback-icon-theme = "gnome"
        gtk-font-name = "DejaVu Sans 11"
      '';
      mode = "444";
    };
    etc."xdg/gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-icon-theme-name=Arc
        gtk-theme-name=Arc-Dark
        gtk-cursor-theme-name=Adwaita
        gtk-fallback-icon-theme=gnome
        gtk-font-name = Noto Sans 11
      '';
      mode = "444";
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

      # Configure i3-gaps as WM.
      windowManager = {
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          # i3 dependencies.
          extraPackages = with pkgs; [
            dex
            dunst
            i3lock
            i3pyblocks
            kbdd
            libnotify
            maim
            nitrogen
            rofi
            unstable.picom
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
  };
}
