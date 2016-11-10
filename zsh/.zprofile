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
  path=(
    $NPM_PACKAGES/bin
    $path
  )
fi

# Check if ruby-gem is installed, if yes then import it to PATH
if type gem > /dev/null; then
  export GEM_HOME="$HOME/.gem"
  path=(
    $GEM_HOME/ruby/*/bin
    $path
  )
fi
