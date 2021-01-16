if [[ "${TERM}" = "xterm-kitty" ]]; then
  _kitty() {
    local src
    # Send all words up to the word the cursor is currently on
    src=$(printf "%s " "${(@)words[1,$CURRENT]}" | kitty +complete zsh)
    if [[ $? == 0 ]]; then
      eval ${src}
    fi
  }

  copy() { kitty +kitten clipboard ${@} }
  diffk() { kitty +kitten diff ${@} }
  icat() { kitty +kitten icat ${@} }
  paste() { kitty +kitten clipboard --get-clipboard ${@} }
  ssh() { kitty +kitten ssh ${@} }

  compdef _kitty kitty
fi
