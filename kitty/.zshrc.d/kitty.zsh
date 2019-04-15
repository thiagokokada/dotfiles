_kitty() {
  local src
  # Send all words up to the word the cursor is currently on
  src=$(printf "%s " "${(@)words[1,$CURRENT]}" | kitty +complete zsh)
  if [[ $? == 0 ]]; then
    eval ${src}
  fi
}

if [[ $TERM = "xterm-kitty" ]]; then
  alias icat="kitty +kitten icat"
  alias sshk="kitty +kitten ssh"
  alias d="kitty +kitten diff"
  compdef _kitty kitty
fi
