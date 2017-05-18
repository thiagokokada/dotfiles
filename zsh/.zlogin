# Source a zlogin file depending of the current TTY
# Example: /home/m45t3r/.tty1_zlogin
if [ -z "$DISPLAY" ]; then
  TTY_LOGIN="$HOME/.tty${XDG_VTNR}_login.zsh"
  [ -f "$TTY_LOGIN" ] && source "$TTY_LOGIN"
fi
