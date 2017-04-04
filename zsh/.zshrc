# source zplug configuration
source ~/.zplugrc

# post configuration

# for vi mode
export KEYTIMEOUT=1

# fzf
export FZF_CTRL_T_COMMAND="find . -not -path '*/\.*' -printf '%P\n'"
export FZF_ALT_C_COMMAND="find . -not -path '*/\.*' -type d -printf '%P\n'"

# history-substring-search
bindkey "${terminfo[kcuu1]}" history-substring-search-up
bindkey "${terminfo[kcud1]}" history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# zsh-autosuggestions
bindkey '^ ' autosuggest-accept
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

# edit current line
autoload -Uz edit-command-line
bindkey '^v' edit-command-line

# powerlevel9k
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir rbenv virtualenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(vi_mode background_jobs status)

# aliases
alias open="open_command"
alias charginmahlazer="source ~/.zshrc"
alias nvimdiff="nvim -d"
alias gk="gitk &>/dev/null &"
alias archup="pacaur -Syu --devel --needed"

# allow VTE based terminals to open new tabs on current directory
if [ -f "/etc/profile.d/vte.sh" ]; then
  source "/etc/profile.d/vte.sh"
fi

# source .zshrc.local, if exists
if [ -f "$HOME/.zshrc.local" ]; then
  source "$HOME/.zshrc.local"
fi
