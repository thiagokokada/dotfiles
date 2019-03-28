ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/syl20bnr/spacemacs#develop" ".emacs.d"

alias em="run-bg emacs"

em() { close-fd emacsclient -a '' -nqc "${@}" }
