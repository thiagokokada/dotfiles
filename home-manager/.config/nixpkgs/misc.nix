{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super:
      {
        unstable = import
          (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {
            config = super.config;
          };
      }
    )
  ];

  home = {
    sessionVariables = {
      DOTFILES_PATH = "$HOME/.dotfiles";
    };

    activation.systemdUserResetFailed = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      systemctl --user reset-failed
    '';
  };
}
