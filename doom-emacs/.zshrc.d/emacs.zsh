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
DOOM_SNAPHOST_HASH_PATH="${HOME}/.doom-snapshot-hash"

doom-snapshot-and-upgrade() {
  git --git-dir "${EMACS_PATH}/.git" rev-parse HEAD >! "${DOOM_SNAPHOST_HASH_PATH}"
  emacs -f straight-freeze-versions -f kill-emacs
  doom -y upgrade
}

doom-restore() {
  if [[ -f "${DOOM_SNAPHOST_HASH_PATH}" ]]; then
    git --git-dir "${EMACS_PATH}/.git" reset $(cat "${DOOM_SNAPHOST_HASH_PATH}")
  fi
  if [[ -f "${EMACS_PATH}/.local/straight/versions/default.el" ]]; then
    emacs -f straight-thaw-versions -f kill-emacs
  fi
  doom -y refresh
  doom rebuild -f
}

UPGRADE_CMDS+="doom-snapshot-and-upgrade"
