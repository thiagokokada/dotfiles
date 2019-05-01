{ config, lib, pkgs, ... }:

let
  waylandOverlay = (import (builtins.fetchTarball
  "https://github.com/colemickens/nixpkgs-wayland/archive/master.tar.gz"
  ));

  unstable = import (builtins.fetchGit {
    name = "nixos-unstable-2019-04-16";
    url = https://github.com/nixos/nixpkgs/;
    rev = "aea8e5de3845b4f6afe085e59a39e67293683629"; # py3status 3.18
  }) {
    config = config.nixpkgs.config;
  };
in

{
  # Import nixpkgs-wayland overlay.
  nixpkgs.overlays = [ waylandOverlay ];

  environment = {
    systemPackages = with pkgs; [
      gnome3.dconf-editor
      qt5.qtwayland
    ];

    variables = {
      # Export modules to allow PCManFM to use gvfs.
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
    };
  };

  programs = {
    sway = {
      enable = true;
      extraPackages = with pkgs; [
        (callPackage ./pkgs/bemenu.nix {})
        (with unstable.python3Packages;
          (py3status.overrideAttrs (oldAttrs: {
            meta.priority = -1;
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
        # Enable GNOME support to Wayland
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
  };

  services = {
    # Allow automounting.
    gnome3.gvfs.enable = true;
  };
}
