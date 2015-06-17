#########
# Hacks #
#########

# Disable prompt from grml-zsh
command -v prompt &> /dev/null && prompt off

# Emulate Mac OSX's open
open() {
  if [[ ${1} = "-h" ]] || [[ -z "${@}" ]]; then
    echo "usage: ${0} FILE [FILE ...]" 1>&2
    return 1
  else
    for file in "${@}"; do
      if [[ -f "${file}" ]]; then
        xdg-open "${file}" &> /dev/null &
      else
        echo "File '"${file}"' does not exist!" 1>&2
        return 1
      fi
    done
  fi
}

################
# Prezto setup #
################

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

######################
# User configuration #
######################

# Aliases
alias zshconfig="gvim ~/.zshrc"
alias oh-my-zsh="cd ~/.oh-my-zsh"
alias vimconfig="gvim ~/.vimrc"
alias gvimconfig="gvim ~/.gvimrc"
alias i3config="gvim ~/.config/i3/config"
alias updateplugins="cd ~/.dotfiles; git submodule update --init --recursive --remote; cd -"
alias charginmahlazer="source ~/.zshrc"

# The next line updates PATH for the Google Cloud SDK.
source '/home/m45t3r/Source/google-cloud-sdk/path.zsh.inc'
# The next line enables bash completion for gcloud.
source '/home/m45t3r/Source/google-cloud-sdk/completion.zsh.inc'
