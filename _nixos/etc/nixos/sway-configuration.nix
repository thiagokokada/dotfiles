{ pkgs, ... }:

{
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
        (callPackage ./pkgs/wdisplays.nix {})
        (redshift.overrideAttrs (oldAttrs: rec {
          src = fetchFromGitHub {
            owner = "minus7";
            repo = "redshift";
            rev = "7da875d34854a6a34612d5ce4bd8718c32bec804";
            sha256 = "0nbkcw3avmzjg1jr1g9yfpm80kzisy55idl09b6wvzv2sz27n957";
          };
          buildInputs = oldAttrs.buildInputs ++ [
            wayland wayland-protocols wlroots
          ];
          meta.priority = -1;
        }))
        bemenu
        dmenu
        grim
        i3status-rust
        j4-dmenu-desktop
        libnotify
        lm_sensors
        maim
        mako
        pavucontrol
        playerctl
        slurp
        swayidle
        swaylock
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
    gvfs.enable = true;
  };
}
