ZIT_MODULES_PATH="${HOME}" zit-in "https://github.com/syl20bnr/spacemacs#develop" ".emacs.d"

em() {
  if emacsclient -ne "()" &> /dev/null; then
    emacsclient -nqc "$@" &> /dev/null
  else
    emacsclient -a "" -nqc "$@" &> /dev/null
  fi

}
