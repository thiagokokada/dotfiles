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

alias em="GDK_BACKEND=x11 run-bg emacs"
alias et="emacs -nw"
alias ec="GDK_BACKEND=x11 close-fd emacsclient -a '' -nqc"

install-clojure-lsp() {
  local clojure_lsp_path="${HOME}/.local/bin/clojure-lsp"
  local releases_url="https://api.github.com/repos/snoe/clojure-lsp/releases/latest"
  local download_url="$(curl -s "${releases_url}" | jq -r '[.assets[]][0].browser_download_url')"
  echo "${download_url}"

  curl -L "${download_url}" -o "${clojure_lsp_path}"
  chmod +x "${clojure_lsp_path}"
}

install-clojure-kondo() {
  local clojure_kondo_path="${HOME}/.local/bin/clj-kondo"
  local releases_url="https://api.github.com/repos/borkdude/clj-kondo/releases/latest"
  # TODO: get static binary using jq's query
  local download_url="$(curl -s "${releases_url}" | jq -r '[.assets[]][1].browser_download_url')"
  local tempdir=$(mktemp -d)
  #trap "rm -rf "${tempdir}"" EXIT
  local tempfile="${tempdir}/clj-kondo.zip"

  echo "${download_url}"
  curl -L "${download_url}" -o "${tempfile}"
  unzip "${tempfile}" -d "${tempdir}"
  mv "${tempdir}/clj-kondo" "${clojure_kondo_path}"
  chmod +x "${clojure_kondo_path}"
}

UPGRADE_CMDS+="doom upgrade -f"
