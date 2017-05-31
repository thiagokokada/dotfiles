# source zplug configuration
source ~/.zplugrc

# post configuration

# pager
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"

# prezto
zstyle ':prezto:module:ssh:load' identities '/dev/null'

# fzf
export FZF_CTRL_T_COMMAND="find . -not -path '*/\.*' -printf '%P\n'"
export FZF_ALT_C_COMMAND="find . -not -path '*/\.*' -type d -printf '%P\n'"

# history-substring-search
bindkey "${terminfo[kcuu1]}" history-substring-search-up
bindkey "${terminfo[kcud1]}" history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# zsh-autosuggestions
export ZSH_AUTOSUGGEST_USE_ASYNC=true
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
alias grep="grep --color=auto"
alias http-server="python3 -m http.server"
alias ls="ls --color=auto"
alias nvimdiff="nvim -d"
alias open="xdg-open"
alias rg="rg -g '!*.min.*'"
alias ssh="TERM=xterm ssh"

# source contents from ~/.zshrc.d
for file in $HOME/.zshrc.d/*; do
  source "$file"
done
