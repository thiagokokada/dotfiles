#
# Global variables
#
export EDITOR="nvim"

# Added .pythonrc only if it exists
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
  ~/.zplug/bin
  ~/.local/bin
  $path
)

# Check if npm is installed, if yes make -g install location in user directory
if type npm > /dev/null; then
  export NPM_PACKAGES="$HOME/.npm-packages"
  export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
  export PATH="$NPM_PACKAGES/bin:$PATH"
fi

# Check if ruby-gem is installed, if yes then import it to PATH
if type gem > /dev/null; then
  export GEM_HOME="$HOME/.gem"
  export PATH="$GEM_HOME/ruby/*/bin:$PATH"
fi

# Auto start X11, providing kind of a primitive display manager
if [ -z "$DISPLAY" ]; then
  case "$(fgconsole)" in
    1)
      [ -f "$HOME/.xinitrc" ] && exec startx
      ;;
    2)
      [ -f "/usr/bin/sway" ] && exec gnome-session
      ;;
  esac
fi
