alias icat="kitty +kitten icat"

if [[ $TERM = "xterm-kitty" ]]; then
  source <(kitty + complete setup zsh)
fi
