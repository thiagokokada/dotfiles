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

install-clojure-lsp() {
  local clojure_lsp_path="${HOME}/.local/bin/clojure-lsp"
  local releases_url="https://api.github.com/repos/snoe/clojure-lsp/releases/latest"
  local download_url="$(curl -s "${releases_url}" | jq -r '[.assets[]][0].browser_download_url')"
  curl -L "${download_url}" -o "${clojure_lsp_path}"
  chmod +x "${clojure_lsp_path}"
}

EMACS_PATH="${HOME}/.emacs.d"

UPGRADE_CMDS+="doom upgrade -f"
