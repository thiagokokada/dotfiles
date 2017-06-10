###############################################################################
# zit - minimal module loader for zsh for git modules                         #
###############################################################################
# * In your ~/.zshrc:                                                         #
# source ~/.zit.zsh                                                           #
# zit-install-load "https://github.com/Eriner/zim/" ".zim"                    #
# zit-install-load "https://github.com/zsh-users/zsh-autosuggestions" \       #
# ".zit.d/zsh-autosuggestions" "zsh-autosuggestions.zsh"                      #
# zit-install "https://github.com/Tarrasch/zsh-autoenv" ".zit.d/zsh-autoenv   #
# zit-load ".zit.d/zsh-autoenv" "autoenv.zsh"                                 #
#                                                                             #
# * To update your plugins, run in interactive session:                       #
# $ zit-update                                                                #
###############################################################################

# store all loaded modules to paths
declare -a ZIT_MODULES_LOADED
# set variable below to change zit modules path
if [[ ! -v ZIT_MODULES_PATH ]]; then
  export ZIT_MODULES_PATH=${ZDOTDIR:-${HOME}}
fi

# loader
zit-load() {
  local module_dir="${ZIT_MODULES_PATH}/${1}"
  local dot_zsh="${2:-*.zsh}"
  # load module in zsh
  eval source "${module_dir}/${dot_zsh}"
  # added to global dir array for updater
  ZIT_MODULES_LOADED+=("${module_dir}")
}

# installer
zit-install() {
  local git_repo="${1}"
  local module_dir="${ZIT_MODULES_PATH}/${2}"

  # clone module
  if [[ ! -d "${module_dir}" ]]; then
    echo "Installing ${module_dir}" && \
    git clone --recursive "${git_repo}" "${module_dir}" && \
    echo
  fi
}

# do both above in one step
zit-install-load() {
  local git_repo="${1}"
  local module_dir="${2}"
  local dot_zsh="${3:-*.zsh}"

  zit-install "${git_repo}" "${module_dir}"
  zit-load "${module_dir}" "${dot_zsh}"
}

# updater
zit-update() {
  for module in ${ZIT_MODULES_LOADED}; do
    pushd "${module}"
    echo "Updating ${module}"
    git pull --rebase
    echo
    popd
  done
}

# vim: ft=zsh
