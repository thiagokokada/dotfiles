#########
# Hacks #
#########

# Disable prompt from grml-zsh
command -v prompt &> /dev/null && prompt off

# Start tmux with 256 color support
if command -v tmux &> /dev/null; then
  if [[ -o interactive ]] && [[ -z ${TMUX} ]]; then
    exec tmux -2
  fi
fi

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

#################
# Zgen setup #
#################

# Load antigen
source ${HOME}/.dotfiles/zgen/zgen.zsh

# check if there's no init script
if ! zgen saved; then
  echo "Creating a zgen save"

  # Load robbyrussell's oh-my-zsh's library
  zgen oh-my-zsh

  # Plugins from robbyrussell's oh-my-zsh
  zgen oh-my-zsh plugins/tmux
  zgen oh-my-zsh plugins/git
  zgen oh-my-zsh plugins/pip
  zgen oh-my-zsh plugins/python
  zgen oh-my-zsh plugins/virtualenv
  zgen oh-my-zsh plugins/command-not-found
  zgen oh-my-zsh plugins/history-substring-search

  # Github plugins
  zgen load rupa/z
  zgen load zsh-users/zsh-syntax-highlighting
  zgen load zsh-users/zsh-completions src
  zgen load kennethreitz/autoenv

  # Load theme
  zgen oh-my-zsh themes/ys

  # Tell antigen that you're done
  zgen save

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
source '/home/m45t3r/google-cloud-sdk/path.zsh.inc'
# The next line enables bash completion for gcloud.
source '/home/m45t3r/google-cloud-sdk/completion.zsh.inc'
