{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super:
      rec {
        unstable = import
          (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {
            config = super.config;
          };

        nnn = unstable.nnn.overrideAttrs (oldAttrs: {
          makeFlags = oldAttrs.makeFlags ++ [ "O_NERD=1" ];
        });
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
