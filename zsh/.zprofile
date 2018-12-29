#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path

# Use C style sort
export LC_COLLATE=C

path=(
  ~/.local/bin
  $path
)
