{ config, lib, pkgs, ... }:

{
  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  nixpkgs = {
    overlays = [
      (self: super: rec {
        unstable = import (fetchTarball
          "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {
            config = super.config;
          };

        nnn = unstable.nnn.override { withNerdIcons = true; };
      })
    ];
  };

  home = {
    sessionVariables = { DOTFILES_PATH = "$HOME/.dotfiles"; };

    activation.systemdUserResetFailed =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        systemctl --user reset-failed
      '';
  };

  programs.zsh.shellAliases = {
    "hm" = "home-manager";
    "hm-update" = "nix-channel --update && home-manager switch";
  };
}
