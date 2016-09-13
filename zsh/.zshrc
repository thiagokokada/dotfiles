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
alias zshconfig="vim ~/.zshrc"
if [[ -s "~/.vimrc" ]]; then
  alias vimconfig="vim ~/.vimrc"
  alias gvimconfig="vim ~/.gvimrc"
fi
if [[ -s "~/.config/i3/config" ]]; then
  alias i3config="vim ~/.config/i3/config"
  alias i3status="vim ~/.config/i3status/config"
fi
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
