if [[ $TERM = "xterm-kitty" ]]; then
  alias icat="kitty +kitten icat"
  alias sshk="kitty +kitten ssh"
  alias d="kitty +kitten diff"
  source <(kitty + complete setup zsh)
fi
