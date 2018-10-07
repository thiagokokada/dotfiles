# Command to update everything
archup() {
  zit-update
  (( $+commands[yay] )) && yay
  (( $+commands[flatpak] )) && flatpak update
  (( $+commands[nvim] )) && nvim -c PlugUpdate -c qall
}
