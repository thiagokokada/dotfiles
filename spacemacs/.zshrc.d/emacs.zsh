ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/syl20bnr/spacemacs#develop" ".emacs.d"

em() { close-fd emacsclient -a '' -nqc "${@}" }
