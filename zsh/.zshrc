# load Zit config
source "${HOME}/.zitrc"

# try to correct the spelling of commands
setopt CORRECT

# pager
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"

# fzf
FZF_KEY_BINDINGS="/usr/share/fzf/key-bindings.zsh"
if [[ -s "${FZF_KEY_BINDINGS}" ]]; then
  source "${FZF_KEY_BINDINGS}"
  export FZF_CTRL_T_COMMAND="find . -not -path '*/\.*' -printf '%P\n'"
  export FZF_ALT_C_COMMAND="find . -not -path '*/\.*' -type d -printf '%P\n'"
fi

# zsh-autosuggestions
export ZSH_AUTOSUGGEST_USE_ASYNC=true
bindkey '^ ' autosuggest-accept
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# edit current line
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^v' edit-command-line

# aliases
alias archup="pacaur -Syu --devel --needed"
alias charginmahlazer="source ~/.zshrc"
alias gk="gitk &!"
alias grep="grep --color=auto"
alias http-server="python3 -m http.server"
alias ls="ls --color=auto"
alias nvimdiff="nvim -d"
alias rg="rg -g '!*.min.*'"
alias ssh="TERM=xterm ssh"

open() { xdg-open "${@}" &> /dev/null &! }

# source contents from ~/.zshrc.d
for file in ${HOME}/.zshrc.d/*.zsh; do
  source ${file}
done
