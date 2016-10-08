#
# Global variables
#
export EDITOR='vim'
export KEYTIMEOUT=1
export PYTHONSTARTUP="$HOME/.pythonrc.py"

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  ~/bin
  ~/.local/bin
  $path
)

if which ruby >/dev/null && which gem >/dev/null; then
  PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi
