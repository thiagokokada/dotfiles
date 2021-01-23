{ config, lib, pkgs, ... }:

{
  nixpkgs = {
    config.allowUnfree = true;

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

  my.dotfiles-dir = ../../..;

  home = {
    sessionVariables = { DOTFILES_PATH = "$HOME/.dotfiles"; };

    activation.systemdUserResetFailed =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        systemctl --user reset-failed
      '';
  };
}
