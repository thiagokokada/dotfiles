export NIX_HOME="${DOTFILES_PATH}/_nixos"

nixos-clean-up() {
  sudo -s -- <<EOF
find -H /nix/var/nix/gcroots/auto -type l | xargs readlink | grep "/result$" | xargs rm -f
nix-collect-garbage -d
nixos-rebuild boot --fast
if [[ "$1" == "--optimize" ]]; then
  nix-store --optimize
fi
EOF
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
