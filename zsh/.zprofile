#
# Global variables
#
export EDITOR="nvim"
export VISUAL="$EDITOR"
export PAGER="less"

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

# Check if go is installed, if yes import it to PATH
if type go > /dev/null; then
  export GOPATH="$HOME/.go"
  path=(
    $GOPATH/bin
    $path
  )
fi

# Put gem and rbenv in PATH, if exists
if type gem > /dev/null; then
  path=(
    $HOME/.gem/bin
    $path
  )
fi

if [ -d "$HOME/.rbenv/bin" ]; then
  path=(
    $HOME/.rbenv/bin
    $path
  )
fi
