ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/hlissner/doom-emacs#develop" ".emacs.d"

path=(
  ~/.emacs.d/bin
  ${path}
)

alias em="run-bg emacs"

ec() { close-fd emacsclient -a '' -nqc "${@}" }
