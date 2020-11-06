{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      gnome3.dconf-editor
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
        bemenu
        dmenu
        grim
        i3status-rust
        j4-dmenu-desktop
        maim
        mako
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
}
