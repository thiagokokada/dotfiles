# source zplug configuration
source ~/.zplugrc

# post configuration

# for vi mode
export KEYTIMEOUT=1

# history-substring-search
bindkey "${terminfo[kcuu1]}" history-substring-search-up
bindkey "${terminfo[kcud1]}" history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# zsh-autosuggestions
bindkey '^ ' autosuggest-accept

# powerlevel9k
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir rbenv virtualenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(vi_mode background_jobs status)

# aliases
alias open="xdg-open "$@" &>/dev/null"
alias charginmahlazer="source ~/.zshrc"
alias nvimdiff="nvim -d"

# source .zshrc.local, if exists
if [ -f "$HOME/.zshrc.local" ]; then
  source "$HOME/.zshrc.local"
fi
