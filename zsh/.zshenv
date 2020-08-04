#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path

#
# Environment variables
#

export VISUAL="nvim"
export EDITOR="${VISUAL}"
export PAGER="less"
export TERMINAL="kitty"
export DOTFILES_PATH="${HOME}/.dotfiles"
