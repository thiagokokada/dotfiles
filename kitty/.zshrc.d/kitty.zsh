if [[ $TERM = "xterm-kitty" ]]; then
  alias icat="kitty +kitten icat"
  alias sshk="kitty +kitten ssh"
  source <(kitty + complete setup zsh)
fi
