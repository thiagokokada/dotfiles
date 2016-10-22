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

# Auto start X11, providing kind of a primitive display manager
if [ -z "$DISPLAY" ]; then
  case "$(fgconsole)" in
    1)
      [ -f "$HOME/.xinitrc" ] && exec startx
      ;;
    2)
      [ -f "/usr/bin/sway" ] && exec sway
      ;;
  esac
fi
