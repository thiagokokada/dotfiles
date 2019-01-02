export DOTFILES_NIX_HOME="${HOME}/.dotfiles/_nixos"

alias nix-copy-etc="cp /etc/nixos/^(configuration.nix|hardware-configuration.nix) ${DOTFILES_NIX_HOME}/etc/nixos"

alias nix-shell-fhs="nix-shell ${DOTFILES_NIX_HOME}/fhs-shell.nix"
