# source zplug configuration
source ~/.zplugrc

# post configuration
# vi mode
export KEYTIMEOUT=1

# powerlevel9k
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir rbenv virtualenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs status)

# aliases
alias open="xdg-open "$@" &>/dev/null"
alias charginmahlazer="source ~/.zshrc"
alias nvimdiff="nvim -d"

# source .zshrc.local, if exists
if [ -f "$HOME/.zshrc.local" ]; then
  source "$HOME/.zshrc.local"
fi
