#
# Source Prezto
#

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#
# Google Cloud SDK configuration
#
export CLOUDSDK_PYTHON='python2'
export CLOUDSDK_PATH="$HOME/Source/google-cloud-sdk"

if [[ -d "$CLOUDSDK_PATH" ]]; then
  # The next line updates PATH for the Google Cloud SDK.
  source "$CLOUDSDK_PATH/path.zsh.inc"
  # The next line enables zsh completion for gcloud.
  source "$CLOUDSDK_PATH/completion.zsh.inc"
fi

#
# Aliases
#
alias open="o"
alias zshconfig="gvim ~/.zshrc"
alias vimconfig="gvim ~/.vimrc"
alias gvimconfig="gvim ~/.gvimrc"
alias upgradeplugins="cd ~/.dotfiles; git submodule update --init --recursive --remote; cd -"
alias charginmahlazer="source ~/.zshrc"
alias mpv="noglob mpv"
alias youtube-dl="noglob youtube-dl"
