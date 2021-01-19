{ config, lib, pkgs, ... }:

{
  home = {
    sessionVariables = {
      DOTFILES_PATH = "$HOME/.dotfiles";
    };

    activation.systemdUserResetFailed = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      systemctl --user reset-failed
    '';
  };
}
