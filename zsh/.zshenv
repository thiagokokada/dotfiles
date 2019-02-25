#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path

path=(
  ~/.local/bin
  $path
)

#
# Environment variables
#

# pager
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"
