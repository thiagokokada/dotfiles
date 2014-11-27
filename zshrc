###############
# Workarounds #
###############

# Start tmux
if command -v tmux>/dev/null; then
  [[ ! $TERM =~ screen ]] && [ -z $TMUX ] && exec tmux
fi

# Disable prompt from grml-zsh
command -v prompt &> /dev/null && prompt off

#################
# Antigen setup #
#################

# Load antigen
source $HOME/.dotfiles/antigen/antigen.zsh

# Load robbyrussell's oh-my-zsh's library
antigen use oh-my-zsh

# Plugins from robbyrussell's oh-my-zsh
#antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle command-not-found
antigen bundle history-substring-search

# Github plugins
antigen bundle rupa/z
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions src
antigen bundle kennethreitz/autoenv

# Load theme
antigen theme ys

# Tell antigen that you're done
antigen apply

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

# Environmental vars
export EDITOR="vim"

