{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      arc-icon-theme
      arc-theme
      gnome3.adwaita-icon-theme
      gnome3.dconf-editor
      hicolor-icon-theme
      qt5.qtwayland
      pcmanfm
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

        # Enable wayland in Firefox
        export MOZ_ENABLE_WAYLAND=1

        # Enable Qt5 support to Wayland
        export QT_QPA_PLATFORM=wayland

        # Enable EFL support to Wayland
        export ECORE_EVAS_ENGINE=wayland_egl
        export ELM_ENGINE=wayland_egl

        # Enable SDL support to Wayland
        export SDL_VIDEODRIVER=wayland

        # Fix Java applications
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    };

    # Enable dconf.
    dconf.enable = true;

    # Backlight control.
    light.enable = true;
  };

  # Enable Qt5 integration.
  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
}
