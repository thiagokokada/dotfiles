# load zit
export ZIT_MODULES_PATH="${HOME}/.zit.d"

if [[ ! -d "${ZIT_MODULES_PATH}/zit" ]]; then
  git clone "https://github.com/thiagokokada/zit" "${ZIT_MODULES_PATH}/zit"
fi
source "${ZIT_MODULES_PATH}/zit/zit.zsh"

# let zit manage zit
zit-in "https://github.com/thiagokokada/zit" "zit"

# zim
zdouble_dot_expand='true'
zssh_ids=(/dev/null)
zhighlighters=(main brackets cursor)

zit-in "https://github.com/Eriner/zim" "zim"
zit-lo "zim" "modules/directory/init.zsh"
zit-lo "zim" "modules/environment/init.zsh"
zit-lo "zim" "modules/git/init.zsh"
zit-lo "zim" "modules/history/init.zsh"
zit-lo "zim" "modules/input/init.zsh"
zit-lo "zim" "modules/utility/init.zsh"
zit-lo "zim" "modules/ssh/init.zsh"
zit-lo "zim" "modules/completion/init.zsh"
zit-lo "zim" "modules/syntax-highlighting/init.zsh"
zit-lo "zim" "modules/history-substring-search/init.zsh"
zit-lo "zim" "modules/autosuggestions/init.zsh"

# pure
zit-in "https://github.com/sindresorhus/pure" "pure"
zit-lo "pure" "async.zsh"
zit-lo "pure" "pure.zsh"


# misc
zit-il "https://github.com/hlissner/zsh-autopair" "autopair" "autopair.zsh"
zit-il "https://github.com/Tarrasch/zsh-autoenv" \
  "zsh-autoenv" "autoenv.zsh"

# Set right prompt to show time
export RPROMPT="%F{8}%*"

# try to correct the spelling of commands
setopt correct

# disable Ctrl+S/Ctrl-Q
setopt noflowcontrol

# enable vi-like keys
bindkey -v
export KEYTIMEOUT=1

# zsh-autosuggestions
bindkey '^ ' autosuggest-accept
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# edit current line
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# upgrade-all cmd
export -ua UPGRADE_CMDS=("zit-up")
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
to-string() { awk '{print "\""$0"\""}' }

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
