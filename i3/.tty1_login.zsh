XINITRC="$HOME/.xinitrc"
[ -f "$XINITRC" ] && exec startx "$XINITRC" i3
