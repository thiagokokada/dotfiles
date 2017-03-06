# Auto start X11, providing kind of a primitive display manager
XINITRC="$HOME/.xinitrc"

if [ -z "$DISPLAY" ] && [ -f "$XINITRC" ]; then
  case "$XDG_VTNR" in
    1)
      exec startx "$XINITRC" i3
      ;;
    2)
      exec startx "$XINITRC" gnome-session
      ;;
  esac
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
