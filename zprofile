#
# Global variables
#
export EDITOR='vim'
export CLOUDSDK_PYTHON='python2'

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
  /usr/local/{bin,sbin}
  $path
)

# The next line updates PATH for the Google Cloud SDK.
source '/home/m45t3r/Source/google-cloud-sdk/path.zsh.inc'
# The next line enables zsh completion for gcloud.
source '/home/m45t3r/Source/google-cloud-sdk/completion.zsh.inc'
