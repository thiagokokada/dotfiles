export DOTFILES_NIX_HOME="${HOME}/.dotfiles/_nixos"

alias nix-shell-fhs="nix-shell ${DOTFILES_NIX_HOME}/fhs-shell.nix"

nixos-copy-etc() {
  diff --color=auto -r "${DOTFILES_NIX_HOME}/etc/nixos" /etc/nixos/

  while true; do
    printf '%s' 'Copy current NixOS configuration (y/n)? '
    read yn
    case $yn in
        [Yy]* ) cp /etc/nixos/^(configuration.nix|hardware-configuration.nix) ${DOTFILES_NIX_HOME}/etc/nixos
                break;;
        [Nn]* ) break;;
        * ) echo 'Please answer (y)es or (n)o.';;
    esac
  done
}

nixos-restore-etc() {
  diff --color=auto -r /etc/nixos/ "${DOTFILES_NIX_HOME}/etc/nixos"

  while true; do
    printf '%s' 'Restore NixOS configuration (y/n)? '
    read yn
    case $yn in
        [Yy]* ) sudo cp ${DOTFILES_NIX_HOME}/etc/nixos/*.nix /etc/nixos
                break;;
        [Nn]* ) break;;
        * ) echo 'Please answer (y)es or (n)o.';;
    esac
  done
}

upgrade-all() {
  sudo nixos-rebuild switch --upgrade
  (( $+commands[nvim] )) && nvim -c PlugUpdate -c qall
  zit-update
}
