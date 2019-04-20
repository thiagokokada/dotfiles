# load zit
export ZIT_MODULES_PATH="${HOME}/.zit.d"

if [[ ! -d "${ZIT_MODULES_PATH}/zit" ]]; then
  git clone "https://github.com/thiagokokada/zit" "${ZIT_MODULES_PATH}/zit"
fi
source "${ZIT_MODULES_PATH}/zit/zit.zsh"

# let zit manage zit
zit-in "https://github.com/thiagokokada/zit" "zit"

# zim
zit-in "https://github.com/Eriner/zim" "zim"
zit-lo "zim" "modules/directory/init.zsh"
ztermtitle='%~' && \
  zit-lo "zim" "modules/environment/init.zsh"
zit-lo "zim" "modules/git/init.zsh"
zit-lo "zim" "modules/history/init.zsh"
zdouble_dot_expand='true' && \
  zit-lo "zim" "modules/input/init.zsh"
zit-lo "zim" "modules/utility/init.zsh"
zssh_ids=(/dev/null) && \
  zit-lo "zim" "modules/ssh/init.zsh"
zit-lo "zim" "modules/completion/init.zsh"
zhighlighters=(main brackets cursor) && \
  zit-lo "zim" "modules/syntax-highlighting/init.zsh"
zit-lo "zim" "modules/history-substring-search/init.zsh"
zit-lo "zim" "modules/autosuggestions/init.zsh"

# powerlevel9k
# load only if terminal supports at least 256 colors
if [[ "$(tput colors)" -ge 256 ]]; then
  zit-il "https://github.com/bhilburn/powerlevel9k" \
    "powerlevel9k" "powerlevel9k.zsh-theme"
fi

# misc
zit-il "https://github.com/hlissner/zsh-autopair" "autopair" "autopair.zsh"
zit-il "https://github.com/Tarrasch/zsh-autoenv" \
  "zsh-autoenv" "autoenv.zsh"

# try to correct the spelling of commands
setopt correct

# disable Ctrl+S/Ctrl-Q
setopt noflowcontrol

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

# upgrade-all cmd
export -ua UPGRADE_CMDS=("zit-update")
upgrade-all() {
  for cmd in "${UPGRADE_CMDS[@]}"; do
    printf "\nRunning: %s\n\n" "${cmd}"
    eval "${cmd}"
  done
}
alias up!="upgrade-all"

# helpers
close-fd() { "${@}" </dev/null &>/dev/null }
run-bg() { "${@}" </dev/null &>/dev/null &! }
open() { run-bg xdg-open "${@}" }
try-run() { (( $+commands[${1}] )) && "${@}" }

# aliases
alias clean-zsh-cache="rm -f ${HOME}/.*.zwc"
alias gk="run-bg gitk"
alias http-server="python3 -m http.server"
alias ln-clean-up="rm -- **/*(-@D)"
alias nvimdiff="nvim -d"
alias ssh="TERM=xterm-256color ssh"

# source contents from ~/.zshrc.d/*.zsh
for file in ${HOME}/.zshrc.d/*.zsh; do
  [[ -f "${file}" ]] && source "${file}"
done

# script to compile ZSH files
# should be called last
zit-lo "zit" "extras/compile-zsh-files.zsh"
