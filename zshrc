# Disable prompt from grml-zsh
command -v prompt &> /dev/null && prompt off

# Load antigen
source $HOME/.dotfiles/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle virtualenv
antigen bundle history-substring-search
antigen bundle command-not-found

# Github plugins
antigen bundle rupa/z
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions src
antigen bundle kennethreitz/autoenv

# Load the theme.
antigen theme ys

# Tell antigen that you're done.
antigen apply

# User configuration

# Aliases
alias zshconfig="gvim ~/.zshrc"
alias oh-my-zsh="cd ~/.oh-my-zsh"
alias vimconfig="gvim ~/.vimrc"
alias gvimconfig="gvim ~/.gvimrc"
alias i3config="gvim ~/.config/i3/config"
alias updateplugins="cd ~/.dotfiles; git submodule update --init --recursive --remote; cd -"
alias charginmahlazer="source ~/.zshrc"

# Android configuration
export ANDROID_SDK=$HOME/Source/adt-bundle/sdk
export ANDROID_NDK=$HOME/Source/android-ndk-r9d
export ANDROID_SWT=/usr/share/java

# User preferences
export PATH=$HOME/bin:$HOME/Source/adt-bundle/eclipse:$HOME/Source/android-studio/bin:$ANDROID_SDK/tools/:$ANDROID_SDK/platform-tools/:$PATH
export EDITOR="vim"

