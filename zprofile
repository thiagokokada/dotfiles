#
# Global variables
#
export EDITOR='vim'
export CLOUDSDK_PYTHON='python2'
export CLOUDSDK_PATH="$HOME/Source/google-cloud-sdk"

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

if [ -d $CLOUDSDK_PATH ]; then
  # The next line updates PATH for the Google Cloud SDK.
  source "$CLOUDSDK_PATH/path.zsh.inc"
  # The next line enables zsh completion for gcloud.
  source "$CLOUDSDK_PATH/completion.zsh.inc"
fi
