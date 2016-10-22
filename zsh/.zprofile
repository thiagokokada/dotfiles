#
# Global variables
#
export EDITOR='nvim'
export GEM_HOME=~/.gem
export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

if [ -f "$HOME/.pythonrc.py" ]; then
  export PYTHONSTARTUP="$HOME/.pythonrc.py"
fi

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path

manpath=(
  "$NPM_PACKAGES/share/man:$(manpath)"
)

path=(
  ~/bin
  ~/.local/bin
  $HOME/.gem/ruby/*/bin
  $NPM_PACKAGES/bin
  $path
)

# Auto start X11
if [ -z "$DISPLAY" ] && [ "$(fgconsole)" -eq 1 ] && [ -f "$HOME/.xinitrc" ]; then
  exec startx
fi
