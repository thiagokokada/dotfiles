# Command to update everything
archup() {
  (( $+commands[yay] )) && tmux -2 new-session yay
  (( $+commands[flatpak] )) && flatpak update
  (( $+commands[nvim] )) && nvim -c PlugUpdate -c qall
  zit-update
}

# fzf
FZF_KEY_BINDINGS="/usr/share/fzf/key-bindings.zsh"
if [[ -s "${FZF_KEY_BINDINGS}" ]]; then
  source "${FZF_KEY_BINDINGS}"
fi
