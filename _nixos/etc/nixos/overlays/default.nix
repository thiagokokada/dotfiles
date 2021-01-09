self: super:

with super;
let
  unstableTarball = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz";
in rec {
  linuxZenWMuQSS = pkgs.linuxPackagesFor (pkgs.linux_zen.override {
    structuredExtraConfig = with lib.kernel; {
      PREEMPT = yes;
      PREEMPT_VOLUNTARY = lib.mkForce no;
      SCHED_MUQSS = yes;
    };
    ignoreConfigErrors = true;
  });

  unstable = import unstableTarball {
    config = pkgs.config;
  };

  picom = unstable.picom;
}
