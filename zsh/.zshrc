# load zit
ZIT_DIR_PATH="${HOME}/.zit"
if [[ ! -d $ZIT_DIR_PATH ]]; then
  git clone "https://github.com/m45t3r/zit" "$ZIT_DIR_PATH"
fi
source "${ZIT_DIR_PATH}/zit.zsh"

# load modules
export ZIT_MODULES_PATH="${HOME}/.zit.d"

# Zim
source "${HOME}/.zimrc"
zit-in "https://github.com/Eriner/zim" "zim"
zit-lo "zim" "modules/directory/init.zsh"
zit-lo "zim" "modules/environment/init.zsh"
zit-lo "zim" "modules/git/init.zsh"
zit-lo "zim" "modules/git-info/init.zsh"
zit-lo "zim" "modules/history/init.zsh"
zit-lo "zim" "modules/utility/init.zsh"
zit-lo "zim" "modules/ssh/init.zsh"
zit-lo "zim" "modules/syntax-highlighting/init.zsh"
zit-lo "zim" "modules/history-substring-search/init.zsh"
zit-lo "zim" "modules/prompt/init.zsh"
zit-lo "zim" "modules/completion/init.zsh"

# Misx
zit-il "https://github.com/Tarrasch/zsh-autoenv" \
  "zsh-autoenv" "autoenv.zsh"
zit-il "https://github.com/zsh-users/zsh-autosuggestions" \
  "zsh-autosuggestions" "zsh-autosuggestions.zsh"

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
