ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/hlissner/doom-emacs#develop" ".emacs.d"

path=(
  ~/.emacs.d/bin
  ${path}
)

alias em="run-bg emacs"
alias et="TERM=xterm-24bit emacs -nw"

ec() { close-fd emacsclient -a '' -nqc "${@}" }

UPGRADE_CMDS+="doom update && doom refresh"
