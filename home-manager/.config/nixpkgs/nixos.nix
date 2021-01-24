{ config, lib, pkgs, ... }:

let
  nixos-clean-up = pkgs.writeShellScriptBin "nixos-clean-up" ''
    set -eu

    sudo -s -- <<EOF
    nix-store --gc
    nix-collect-garbage -d
    nix-rebuid boot --fast
    EOF
  '';

  nixos-copy-etc = pkgs.writeShellScriptBin "nixos-copy-etc" ''
    set -u

    NIX_CONFIG_PATH="/etc/nixos"
    NIX_DOTFILES_PATH="$DOTFILES_PATH/_nixos/etc/nixos"

    ${pkgs.colordiff}/bin/colordiff -r "$NIX_DOTFILES_PATH" "$NIX_CONFIG_PATH"

    while true; do
      printf '%s' 'Copy current NixOS configuration (y/n)? '
      read yn
      case $yn in
          [Yy]* ) cp -r $NIX_CONFIG_PATH/* "$NIX_DOTFILES_PATH"
                  break;;
          [Nn]* ) break;;
          * ) echo 'Please answer (y)es or (n)o.';;
      esac
    done
  '';

  nixos-restore-etc = pkgs.writeShellScriptBin "nixos-restore-etc" ''
    set -u

    NIX_CONFIG_PATH="/etc/nixos"
    NIX_DOTFILES_PATH="$DOTFILES_PATH/_nixos/etc/nixos"

    ${pkgs.colordiff}/bin/colordiff -r "$NIX_CONFIG_PATH" "$NIX_DOTFILES_PATH"

    while true; do
      printf '%s' 'Restore NixOS configuration (y/n)? '
      read yn
      case $yn in
          [Yy]* ) sudo cp -r $NIX_DOTFILES_PATH/* "$NIX_CONFIG_PATH"
                  break;;
          [Nn]* ) break;;
          * ) echo 'Please answer (y)es or (n)o.';;
      esac
    done
  '';
in {
  home.packages = [
    nixos-clean-up
    nixos-copy-etc
    nixos-restore-etc
  ];

  programs.zsh.shellAliases = {
    "hm" = "home-manager";
  };
}
