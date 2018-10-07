# Command to update everything
archup() {
  (( $+commands[yay] )) && yay
  (( $+commands[flatpak] )) && flatpak update
  (( $+commands[nvim] )) && nvim -c PlugUpdate -c qall
  zit-update
}
