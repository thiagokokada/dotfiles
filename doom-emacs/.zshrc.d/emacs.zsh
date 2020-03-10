ZIT_MODULES_PATH="${HOME}" ZIT_DISABLE_UPGRADE=1 \
  zit-in "https://github.com/hlissner/doom-emacs#develop" ".emacs.d"

path=(
  ~/.emacs.d/bin
  ${path}
)

if [[ -n "${EMACS}" ]]; then
  export VISUAL="emacsclient"
  export EDITOR="${VISUAL}"
  export GIT_EDITOR="${EDITOR}"
  # Disable RPROMPT in Emacs term
  export RPROMPT=""
fi

alias em="GDK_BACKEND=x11 run-bg emacs"
alias et="TERM=xterm-24bit emacs -nw"
alias ec="GDK_BACKEND=x11 close-fd emacsclient -a '' -nqc"

EMACS_PATH="${HOME}/.emacs.d"

UPGRADE_CMDS+="doom upgrade -f"
