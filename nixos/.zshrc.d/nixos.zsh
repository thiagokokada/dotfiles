export NIX_HOME="${DOTFILES_PATH}/_nixos"

if [[ -n "${commands[fzf-share]}" ]]; then
  source "$(fzf-share)/key-bindings.zsh"
fi

alias nixos-clean-up="sudo -- sh -c 'nix-collect-garbage -d && nixos-rebuild boot'"

nixos-copy-etc() {
  diff --color=auto -r "${NIX_HOME}/etc/nixos" /etc/nixos/

  while true; do
    printf '%s' 'Copy current NixOS configuration (y/n)? '
    read yn
    case $yn in
        [Yy]* ) cp -r /etc/nixos/^(configuration.nix|hardware-configuration.nix) ${NIX_HOME}/etc/nixos
                break;;
        [Nn]* ) break;;
        * ) echo 'Please answer (y)es or (n)o.';;
    esac
  done
}

nixos-restore-etc() {
  diff --color=auto -r /etc/nixos/ "${NIX_HOME}/etc/nixos"

  while true; do
    printf '%s' 'Restore NixOS configuration (y/n)? '
    read yn
    case $yn in
        [Yy]* ) sudo cp -r ${NIX_HOME}/etc/nixos/*.nix /etc/nixos
                break;;
        [Nn]* ) break;;
        * ) echo 'Please answer (y)es or (n)o.';;
    esac
  done
}

UPGRADE_CMDS+="sudo nixos-rebuild switch --upgrade"
