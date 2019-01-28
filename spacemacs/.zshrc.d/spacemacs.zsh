alias ec="emacsclient"
ecw() { sh -c "emacsclient --create-frame ${@} </dev/null &>/dev/null &" }
