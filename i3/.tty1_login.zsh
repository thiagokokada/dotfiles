if (( $+commands[xinit] )); then
  exec xinit i3 -- ":${XDG_VTNR}" "vt${XDG_VTNR}" -keeptty
fi
