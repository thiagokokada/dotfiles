{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: rec {
      unstable = import (fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {
          config = super.config;
        };

      lockscreen = with config.my.fonts;
        pkgs.writeScriptBin "lock-screen" ''
          #!${pkgs.stdenv.shell}

          export XSECURELOCK_FORCE_GRAB=2
          export XSECURELOCK_BLANK_DPMS_STATE="off"
          export XSECURELOCK_DATETIME_FORMAT="%H:%M:%S - %a %d/%m"
          export XSECURELOCK_SHOW_DATETIME=1
          export XSECURELOCK_SHOW_HOSTNAME=0
          export XSECURELOCK_SHOW_USERNAME=0
          export XSECURELOCK_FONT="${gui.name}:style=Regular"

          exec ${pkgs.xsecurelock}/bin/xsecurelock $@
        '';

      nnn = unstable.nnn.override { withNerdIcons = true; };
    })
  ];

  home = {
    sessionVariables = { DOTFILES_PATH = "$HOME/.dotfiles"; };

    activation.systemdUserResetFailed =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        systemctl --user reset-failed
      '';
  };
}
