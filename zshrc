#
# Source Prezto
#

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
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
