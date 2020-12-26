{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/thiagokokada/i3pyblocks/archive/nix-overlay.tar.gz;
    }))
  ];

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

      # Configure i3-gaps as WM.
      windowManager = {
        i3 = {
          enable = true;
          package = pkgs.unstable.i3-gaps;
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

  # User services
  systemd.user.services = {
    gammastep = let
      configFile = pkgs.writeText "config.ini" ''
        [general]
        temp-day=5500
        temp-night=3700
        fade=1
        gamma=0.8
        dawn-time=6:00-7:45
        dusk-time=18:35-20:15
      '';
    in {
      description = "Gammastep color temperature adjuster";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.unstable.gammastep}/bin/gammastep-indicator -c ${configFile} -P";
        RestartSec = 3;
        Restart = "on-failure";
      };
    };

    picom = let
      configFile = pkgs.writeText "picom.conf" ''
        # Fading
        fading = true;
        fade-in-step = 0.15;
        fade-out-step = 0.15;

        # Other
        backend = "glx";
        vsync = true;

        # GLX backend
        glx-no-stencil = true;
        glx-no-rebind-pixmap = true;
        use-damage = true;
      '';
    in {
      description = "Picom composite manager";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.unstable.picom}/bin/picom --config ${configFile} --experimental-backends";
        RestartSec = 3;
        Restart = "on-failure";
      };
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
