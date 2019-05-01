{ config, lib, pkgs, ... }:

let
  url = "https://github.com/colemickens/nixpkgs-wayland/archive/master.tar.gz";
  waylandOverlay = (import (builtins.fetchTarball url));
in

{
  nixpkgs.overlays = [ waylandOverlay ];

  environment.systemPackages = with pkgs; [
    glib
    qt5.qtwayland
  ];

  programs = {
    sway = {
      enable = true;
      extraPackages = with pkgs; [
        (callPackage ./pkgs/bemenu.nix {})
        (with python3Packages;
          (py3status.overrideAttrs (oldAttrs: {
            propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
              i3ipc
              pydbus
              pygobject3
            ];
          })))
        (redshift-wayland.overrideAttrs (oldAttrs: {
          meta.priority = -1;
        }))
        dex
        dmenu
        grim
        libnotify
        lm_sensors
        maim
        mako
        nitrogen
        pavucontrol
        playerctl
        rofi
        slurp
        swayidle
        swaylock
        xdg-desktop-portal-wlr
        xwayland
      ];
      extraSessionCommands = ''
        # Enable GNOME support to Wayland (not everything works well)
        # export GDK_BACKEND=wayland
        # export CLUTTER_BACKEND=wayland

        # Enable Qt5 support to Wayland
        export QT_QPA_PLATFORM=wayland-egl
        export QT_QPA_PLATFORMTHEME=gtk2
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=1

        # Enable EFL support to Wayland
        export ECORE_EVAS_ENGINE=wayland_egl
        export ELM_ENGINE=wayland_egl

        # Enable SDL support to Wayland
        export SDL_VIDEODRIVER=wayland

        # Fix Java applications
        export _JAVA_AWT_WM_NONREPARENTING=1

        # Fix tray icons
        export XDG_CURRENT_DESKTOP=Unity
      '';
    };

    # Enable dconf.
    dconf.enable = true;

    # Backlight control.
    light.enable = true;

    # Enable Qt5 integration.
    qt5ct.enable = true;
  };
}
