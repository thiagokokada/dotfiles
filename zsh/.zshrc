# load Zit config
source "${HOME}/.zitrc"

# try to correct the spelling of commands
setopt correct

# pager
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"

# powerlevel9k
export POWERLEVEL9K_VI_INSERT_MODE_STRING=""
export POWERLEVEL9K_VI_COMMAND_MODE_STRING="NORMAL"
export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(vi_mode dir vcs)
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs)

# fzf
FZF_KEY_BINDINGS="/usr/share/fzf/key-bindings.zsh"
if [[ -s "${FZF_KEY_BINDINGS}" ]]; then
  source "${FZF_KEY_BINDINGS}"
  export FZF_CTRL_T_COMMAND="find . -not -path '*/\.*' -printf '%P\n'"
  export FZF_ALT_C_COMMAND="find . -not -path '*/\.*' -type d -printf '%P\n'"
fi

# vi-mode
bindkey -v

# zsh-autosuggestions
bindkey '^ ' autosuggest-accept
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# edit current line
zle -N edit-command-line
autoload -Uz edit-command-line
bindkey -M vicmd 'v' edit-command-line

# aliases
alias gk="gitk &!"
alias http-server="python3 -m http.server"
alias nvimdiff="nvim -d"
alias ssh="TERM=xterm-256color ssh"
alias ln-clean-up="find -L . -name . -o -type d -prune -o -type l -exec rm {} +"

open() { xdg-open "${@}" &> /dev/null &! }

# source contents from ~/.zshrc.d
for file in ${HOME}/.zshrc.d/*.zsh; do
  source ${file}
done
