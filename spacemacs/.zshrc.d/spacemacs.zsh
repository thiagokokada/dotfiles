ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/syl20bnr/spacemacs#develop" ".emacs.d"

alias ec="emacsclient"
ecw() { run-bg emacsclient --create-frame ${@} }
