###############
# Workarounds #
###############

# Start tmux
if command -v tmux>/dev/null; then
  [[ ! ${TERM} =~ screen ]] && [ -z ${TMUX} ] && exec tmux
fi

# Disable prompt from grml-zsh
command -v prompt &> /dev/null && prompt off

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
  zgen oh-my-zsh plugins/git
  zgen oh-my-zsh plugins/pip
  zgen oh-my-zsh plugins/python
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

