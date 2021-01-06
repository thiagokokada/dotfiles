self: super:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz;
  pkgs = super.pkgs;
  lib = super.lib;
in rec {
  unstable = import unstableTarball {
    config = super.pkgs.config;
  };

  linuxZenWMuQSS = pkgs.linuxPackagesFor (pkgs.linux_zen.override {
    structuredExtraConfig = with lib.kernel; {
      PREEMPT = yes;
      PREEMPT_VOLUNTARY = lib.mkForce no;
      SCHED_MUQSS = yes;
    };
    ignoreConfigErrors = true;
  });

  # Backport unstable packages used in modules
  opentabletdriver = unstable.opentabletdriver;

  picom = unstable.picom;
}
