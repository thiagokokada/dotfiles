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
export VISUAL="nvim"
export EDITOR="${VISUAL}"
export PAGER="less"
export DOTFILES_PATH="${HOME}/.dotfiles"
