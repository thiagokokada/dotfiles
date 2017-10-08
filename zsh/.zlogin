# Simplified version of startx
runx() {
  local program="${1}"
  local tty="${XDG_VTNR}"
  local display=":${tty}"

  exec xinit "${program}" -- "${display}" "vt${XDG_VTNR}" -keeptty &>! "/tmp/tty${tty}_login.log"
}

# Source a zlogin file depending of the current TTY
# Example: /home/m45t3r/.tty1_zlogin
if [[ -z "${DISPLAY}" ]]; then
  TTY_LOGIN="${HOME}/.tty${XDG_VTNR}_login.zsh"
  [[ -f "${TTY_LOGIN}" ]] && source "${TTY_LOGIN}"
fi
