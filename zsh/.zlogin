# Auto start X11, providing kind of a primitive display manager
XINITRC="$HOME/.xinitrc"

if [ -z "$DISPLAY" ]; then
  case "$XDG_VTNR" in
    1)
      [ -f "$XINITRC" ] && exec startx "$XINITRC" i3
      ;;
    2)
      export QT_QPA_PLATFORM=wayland
      exec sway
      ;;
    3)
      export QT_QPA_PLATFORM=wayland
      exec dbus-run-session -- gnome-shell --display-server --wayland
      ;;
  esac
fi
