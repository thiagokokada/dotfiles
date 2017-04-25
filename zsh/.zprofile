#
# Global variables
#
export EDITOR="nvim"
export VISUAL="$EDITOR"
export PAGER="less"

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path

path=(
  ~/.local/bin
  $path
)
