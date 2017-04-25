export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
manpath=(
  $NPM_PACKAGES/share/man
  $manpath
)
path=(
  $NPM_PACKAGES/bin
  $path
)
