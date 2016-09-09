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
alias updateplugins="cd ~/.dotfiles; git submodule update --remote; cd -"
alias initplugins="cd ~/.dotfiles; git submodule update --init --recursive; cd -"
alias charginmahlazer="source ~/.zshrc"
alias mpv="noglob mpv"
alias youtube-dl="noglob youtube-dl"

#
# Source local changes (not commited in repo)
#
if [[ -s "${HOME}/.zshrc.local" ]]; then
  source "${HOME}/.zshrc.local"
fi
