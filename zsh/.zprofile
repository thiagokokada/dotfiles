#
# Global variables
#
export EDITOR='nvim'
export GEM_HOME=~/.gem

if [ -f "$HOME/.pythonrc.py" ]; then
  export PYTHONSTARTUP="$HOME/.pythonrc.py"
fi

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

# Auto start X11
if [ -z "$DISPLAY" ] && [ "$(fgconsole)" -eq 1 ] && [ -f "$HOME/.xinitrc" ]; then
  exec startx
fi
