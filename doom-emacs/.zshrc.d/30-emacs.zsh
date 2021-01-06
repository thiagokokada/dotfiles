EMACS_PATH="${HOME}/.config/emacs"

ZIT_MODULES_PATH="${HOME}/.config" ZIT_DISABLE_UPGRADE=1 \
  zit-in "https://github.com/hlissner/doom-emacs#develop" "emacs"

path=(
  ${EMACS_PATH}/bin
  ${path}
)

if [[ -n "${EMACS}" ]]; then
  export VISUAL="emacsclient"
  export EDITOR="${VISUAL}"
  export GIT_EDITOR="${EDITOR}"
  # Disable RPROMPT in Emacs term
  export RPROMPT=""
fi

alias em="run-bg emacs"
alias et="emacs -nw"
alias ec="close-fd emacsclient -a '' -nqc"

emp() {
  local p
  for p in ${@}; do
    if [[ -d "${p}" ]]; then
      touch "${p}"/.projectile
    elif [[ -f "${p}" ]]; then
      touch $(dirname "${p}")/.projectile
    fi
  done
  em ${@}
}

UPGRADE_CMDS+="doom upgrade -f"
