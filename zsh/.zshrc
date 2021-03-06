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

archive() { zsh "${ZIT_MODULES_PATH}/archive/functions/archive" "${@}" }
unarchive() { zsh "${ZIT_MODULES_PATH}/archive/functions/unarchive" "${@}" }

# zsh-users
zit-il "https://github.com/zsh-users/zsh-autosuggestions" \
  "zsh-autosuggestions" "zsh-autosuggestions.plugin.zsh"

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

# pure
prompt_pure_set_title() { return } # monkeypatch fn to disable title update

# zsh-autosuggestions
ZSH_AUTOSUGGEST_USE_ASYNC=1
bindkey '^ ' autosuggest-accept
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# edit current line
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# helpers
close-fd() { "${@}" </dev/null &>/dev/null }
run-bg() { "${@}" </dev/null &>/dev/null &! }
open() { run-bg xdg-open "${@}" }
get-ip() { curl -Ss "https://ifconfig.me" }
get-ip!() { curl -Ss "https://ipapi.co/$(get-ip)/yaml" }
restart() { pkill "${1}"; run-bg "${@}" }

# upgrade-all cmd
export -ua UPGRADE_CMDS=("dotfiles-pull" "zit-up" "reload!")
upgrade-all() {
  for cmd in "${UPGRADE_CMDS[@]}"; do
    printf "\nRunning: %s\n" "${cmd}"
    eval "${cmd}"
  done
}
alias up!="upgrade-all"

# aliases
alias dotfiles="cd \"${DOTFILES_PATH}\""
alias dotfiles-pull="git -C \"${DOTFILES_PATH}\" pull"
alias reload!="source \"${HOME}/.zshrc\""
alias clean-cache!="rm -f \"${HOME}/.*.zwc\""
alias ln-clean-up="rm -- **/*(-@D)"

# source contents from ~/.zshrc.d/*.zsh
for file in ${HOME}/.zshrc.d/*.zsh; do
  [[ -f "${file}" ]] && source "${file}"
done

# load after ~/.zshrc.d files to make sure that ~/.local/bin is the first in $PATH
export PATH="${HOME}/.local/bin:${PATH}"

# script to compile ZSH files
# should be called last
zit-lo "zit" "extras/compile-zsh-files.zsh"

# Must be sourced at the end of the file:
# https://github.com/zsh-users/zsh-syntax-highlighting#why-must-zsh-syntax-highlightingzsh-be-sourced-at-the-end-of-the-zshrc-file
zit-il "https://github.com/zsh-users/zsh-syntax-highlighting" \
  "zsh-syntax-highlighting" "zsh-syntax-highlighting.plugin.zsh"
zit-il "https://github.com/zsh-users/zsh-history-substring-search" \
  "zsh-history-substring-search" "zsh-history-substring-search.plugin.zsh"

# zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets cursor)

# zsh-history-substring-search
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
