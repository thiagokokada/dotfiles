# load zit
export ZIT_MODULES_PATH="${HOME}/.zit.d"

if [[ ! -d "${ZIT_MODULES_PATH}/zit" ]]; then
  git clone "https://github.com/m45t3r/zit" "${ZIT_MODULES_PATH}/zit"
fi
source "${ZIT_MODULES_PATH}/zit/zit.zsh"

# let zit manage zit
zit-in "https://github.com/m45t3r/zit" "zit"

# zim
export ZIM_HOME="${ZIT_MODULES_PATH}/zim"
zit-il "https://github.com/Eriner/zim" "zim" "init.zsh"

# powerlevel9k
# load only if terminal supports at least 256 colors
if [[ $(tput colors) -ge 256 ]]; then
  zit-il "https://github.com/bhilburn/powerlevel9k" "powerlevel9k" "powerlevel9k.zsh-theme"
fi

# misc
zit-il "https://github.com/supercrabtree/k" "k" "k.sh"
zit-il "https://github.com/Tarrasch/zsh-autoenv" \
  "zsh-autoenv" "autoenv.zsh"

# try to correct the spelling of commands
setopt correct

# disable Ctrl+S/Ctrl-Q
setopt noflowcontrol

# pager
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"

# powerlevel9k
export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs)
export POWERLEVEL9K_PROMPT_ON_NEWLINE=true

# zsh-autosuggestions
bindkey '^ ' autosuggest-accept
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# edit current line
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^v' edit-command-line

# aliases
alias gk="gitk &!"
alias http-server="python3 -m http.server"
alias nvimdiff="nvim -d"
alias ln-clean-up="find -L . -name . -o -type d -prune -o -type l -exec rm {} +"
alias update-plugins="cd ${HOME}/.dotfiles && git submodule update --init --recursive --remote && cd -"

open() { xdg-open "${@}" &> /dev/null &! }

# source contents from ~/.zshrc.d
if [[ -d ${HOME}/.zshrc.d/ ]]; then
  for file in ${HOME}/.zshrc.d/*.zsh; do
    source ${file}
  done
fi

# script to compile ZSH files
# should be called last
zit-lo "zit" "extras/compile-zsh-files.zsh"
