# Auto start X11, providing kind of a primitive display manager
XINITRC="$HOME/.xinitrc"

if [ -z "$DISPLAY" ] || [ -f "$XINITRC" ]; then
  case "$(fgconsole)" in
    1)
      exec startx "$XINITRC" i3
      ;;
    2)
      exec startx "$XINITRC" gnome
      ;;
  esac
fi
