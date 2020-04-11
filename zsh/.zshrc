# load zit
export ZIT_MODULES_PATH="${HOME}/.zit.d"

if [[ ! -d "${ZIT_MODULES_PATH}/zit" ]]; then
  git clone "https://github.com/thiagokokada/zit" "${ZIT_MODULES_PATH}/zit"
fi
source "${ZIT_MODULES_PATH}/zit/zit.zsh"

# let zit manage zit
zit-in "https://github.com/thiagokokada/zit" "zit"

# zim
zstyle ':zim:input' double-dot-expand yes
zstyle ':zim:ssh' ids /dev/null
zstyle ':zim:termtitle' hooks 'precmd' 'preexec'
zstyle ':zim:termtitle' format '%1~'

zit-il "https://github.com/zsh-users/zsh-completions" \
  "zsh-completions" "zsh-completions.plugin.zsh"
zit-il "https://github.com/zimfw/completion" "completion" "init.zsh"
zit-il "https://github.com/zimfw/environment" "environment" "init.zsh"
zit-il "https://github.com/zimfw/input" "input" "init.zsh"
zit-il "https://github.com/zimfw/git" "git" "init.zsh"
zit-il "https://github.com/zimfw/ssh" "ssh" "init.zsh"
zit-il "https://github.com/zimfw/termtitle" "termtitle" "init.zsh"
zit-il "https://github.com/zimfw/utility" "utility" "init.zsh"

zit-in "https://github.com/zimfw/archive" "archive" "init.zsh"

archive() { zsh "${ZIT_MODULES_PATH}/archive/functions/archive" }
unarchive() { zsh "${ZIT_MODULES_PATH}/archive/functions/unarchive" }

# zsh-users
zit-il "https://github.com/zsh-users/zsh-autosuggestions" \
  "zsh-autosuggestions" "zsh-autosuggestions.plugin.zsh"
zit-il "https://github.com/zsh-users/zsh-syntax-highlighting" \
  "zsh-syntax-highlighting" "zsh-syntax-highlighting.plugin.zsh"
zit-il "https://github.com/zsh-users/zsh-history-substring-search" \
  "zsh-history-substring-search" "zsh-history-substring-search.plugin.zsh"

# pure
zit-in "https://github.com/sindresorhus/pure" "pure"
zit-lo "pure" "async.zsh"
zit-lo "pure" "pure.zsh"

# misc
zit-il "https://github.com/rupa/z/" "z" "z.sh"
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

# zsh-history-substring-search
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

# pure
prompt_pure_set_title() { return } # monkeypatch fn to disable title update

# zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets cursor)

# edit current line
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# helpers
dotfiles() { cd "${DOTFILES_PATH}" }
dotfiles-pull() { git -C "${DOTFILES_PATH}" pull }
close-fd() { "${@}" </dev/null &>/dev/null }
run-bg() { "${@}" </dev/null &>/dev/null &! }
open() { run-bg xdg-open "${@}" }
try-run() { (( $+commands[${1}] )) && "${@}" }
to-string() { awk '{print "\""$0"\""}' }
get-ip() { curl -Ss "https://ifconfig.me" }
get-ip!() { curl -Ss "https://ipapi.co/$(get-ip)/yaml" }

# upgrade-all cmd
export -ua UPGRADE_CMDS=("dotfiles-pull" "zit-up")
upgrade-all() {
  for cmd in "${UPGRADE_CMDS[@]}"; do
    printf "\nRunning: %s\n\n" "${cmd}"
    eval "${cmd}"
  done
}
alias up!="upgrade-all"

# aliases
alias reload!="source ${HOME}/.zshrc"
alias clean-cache!="rm -f ${HOME}/.*.zwc"
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
