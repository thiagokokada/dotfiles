export NIX_HOME="${DOTFILES_PATH}/_nixos"

alias nixos-clean-up="sudo -- sh -c 'nix-collect-garbage -d && nixos-rebuild boot --fast'"

nix-remove-stray-roots() {
  nix-store --gc --print-roots |\
    awk '{print $1}' |\
    grep /result$ |\
    sudo xargs -I {} rm -I {}
}

nixos-copy-etc() {
  local nix_config_path="/etc/nixos"
  local nix_dotfiles_path="$DOTFILES_PATH/_nixos/etc/nixos"

  diff --color=auto -r "$nix_dotfiles_path" "$nix_config_path"

  while true; do
    printf '%s' 'Copy current NixOS configuration (y/n)? '
    read yn
    case $yn in
        [yy]* ) cp -r $nix_config_path/* "$nix_dotfiles_path"
                break;;
        [Nn]* ) break;;
        * ) echo 'Please answer (y)es or (n)o.';;
    esac
  done
}

nixos-restore-etc() {
  nix_config_path="/etc/nixos"
  nix_dotfiles_path="$DOTFILES_PATH/_nixos/etc/nixos"

  diff --color=auto -r "$nix_config_path" "$nix_dotfiles_path"

  while true; do
    printf '%s' 'Restore NixOS configuration (y/n)? '
    read yn
    case $yn in
        [yy]* ) sudo cp -r $nix_dotfiles_path/* "$nix_config_path"
                break;;
        [Nn]* ) break;;
        * ) echo 'Please answer (y)es or (n)o.';;
    esac
  done
}

UPGRADE_CMDS+="sudo nixos-rebuild switch --upgrade"
