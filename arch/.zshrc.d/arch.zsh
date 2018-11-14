# Command to update everything
archup() {
  (( $+commands[yay] )) && tmux -2 new-session yay
  (( $+commands[flatpak] )) && flatpak update
  (( $+commands[nvim] )) && nvim -c PlugUpdate -c qall
  zit-update
}
