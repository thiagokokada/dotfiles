ZIT_MODULES_PATH="${HOME}" ZIT_DISABLE_UPGRADE=1 \
  zit-in "https://github.com/hlissner/doom-emacs#develop" ".emacs.d"

path=(
  ~/.emacs.d/bin
  ${path}
)

if [[ -n "${EMACS}" ]]; then
  VISUAL="emacsclient"
  EDITOR="${VISUAL}"
  GIT_EDITOR="${EDITOR}"
fi

alias em="run-bg emacs"
alias et="TERM=xterm-24bit emacs -nw"

ec() { close-fd emacsclient -a '' -nqc "${@}" }

UPGRADE_CMDS+="doom -y upgrade"
