ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/hlissner/doom-emacs#develop" ".emacs.d"

path=(
  ~/.emacs.d/bin
  ${path}
)

em() { close-fd emacsclient -a '' -nqc "${@}" }
