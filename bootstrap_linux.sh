#!/usr/bin/env bash

warn() {
  echo >&2 "${*}"
}

check_dependency () {
  local cmd="${1}"
  command -v "${cmd}" >/dev/null 2>&1 || {
    warn "${cmd} is needed, however it is not installed"
    exit 1
  }
}

set_origin_to_ssh() {
  local current_origin="$(git config --get remote.origin.url)"

  if [[ "${current_origin}" =~ "https://" ]]; then
    echo "Branch 'origin' points to 'https' protocol. Pointing it to 'ssh' instead."
    git remote rm origin
    git remote add origin git@github.com:thiagokokada/dotfiles.git
    git branch --set-upstream-to=origin/master
  fi
}

setup() {
  local name="${1}"
  echo "Configuring ${name}"
  stow "${name}"
}

setup_i3() {
  setup compton
  setup i3
  setup kitty
  setup redshift
  setup termite
}

setup_cli() {
  setup doom-emacs
  setup git
  setup mpv
  setup nvim
  setup ranger
  setup ssh
  setup tig
  setup tmux
  setup youtube-dl
  setup zsh
}

setup_os() {
  . /etc/os-release
  case "${NAME}" in
    "Arch Linux")
      setup arch
      ;;
    "NixOS")
      setup nixos
      ;;
    *)
      warn "No configuration for OS ${NAME} available."
  esac
}

check_dependency "stow"
set_origin_to_ssh
setup_i3
setup_cli
setup_os
