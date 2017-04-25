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
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# edit current line
autoload -Uz edit-command-line
bindkey '^v' edit-command-line

# powerlevel9k
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir rbenv virtualenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(vi_mode background_jobs status)

# aliases
alias archup="pacaur -Syu --devel --needed"
alias charginmahlazer="source ~/.zshrc"
alias gk="gitk &>/dev/null &"
alias http-server="python3 -m http.server"
alias nvimdiff="nvim -d"
alias open="open_command"
alias ssh="TERM=xterm ssh"
alias rg="rg -g '!*.min.*'"

# source contents from ~/.zshrc.d
for file in $HOME/.zshrc.d/*; do
  source "$file"
done
