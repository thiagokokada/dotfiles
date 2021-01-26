{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (fetchGit {
      url = "https://github.com/thiagokokada/i3pyblocks";
      ref = "nix-overlay";
    }))
  ];

  # Configure the virtual console keymap from the xserver keyboard settings
  console.useXkbConfig = true;

  services = {
    # Allow automounting
    gvfs.enable = true;

    # For battery status reporting
    upower.enable = true;

    # Configure gammastep (change screen color over time)
    redshift = {
      enable = true;
      dawnTime = "6:30-7:30";
      duskTime = "18:30-19:30";
      settings = {
        redshift = {
          temp-day = 5500;
          temp-night = 3700;
          gamma = 0.8;
          fade = 1;
        };
      };
      package = pkgs.unstable.gammastep;
      executable = "/bin/gammastep-indicator";
    };

    # Configure picom (for compositing)
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
      # Recommended for modesetting drivers
      useGlamor = true;

      # Enable libinput
      libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
          tapping = true;
        };
        mouse = {
          accelProfile = "flat";
        };
      };

      # Use as default session
      displayManager.defaultSession = "none+i3";

      # Configure i3 as WM
      windowManager = {
        i3 = {
          enable = true;
          package = pkgs.i3;
          # i3 dependencies.
          extraPackages = with pkgs; [
            (rofi.override { plugins = [ rofi-calc rofi-emoji ]; })
            dex
            dunst
            i3lock
            i3pyblocks
            kbdd
            libnotify
            maim
            mons
            nitrogen
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

  # Configure special programs (i.e. hardware access)
  programs = {
    dconf.enable = true;
    light.enable = true;
  };
}
