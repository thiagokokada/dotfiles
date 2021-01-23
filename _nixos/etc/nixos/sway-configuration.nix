{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
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
        # Breaks Chromium/Electron
        # export GDK_BACKEND=wayland
        # Firefox
        export MOZ_ENABLE_WAYLAND=1
        # Qt
        export XDG_SESSION_TYPE=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        # SDL
        export SDL_VIDEODRIVER=wayland
        # Elementary/EFL
        export ECORE_EVAS_ENGINE=wayland_egl
        export ELM_ENGINE=wayland_egl
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    };

    # Enable dconf.
    dconf.enable = true;

    # Backlight control.
    light.enable = true;
  };
}
