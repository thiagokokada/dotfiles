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

sway

logout
