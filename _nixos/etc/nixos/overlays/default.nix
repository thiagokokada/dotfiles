self: super:

let
  unstableTarball = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz";
in rec {
  linux-zen-with-muqss = with super; linuxPackagesFor (linux_zen.override {
    structuredExtraConfig = with lib.kernel; {
      PREEMPT = yes;
      PREEMPT_VOLUNTARY = lib.mkForce no;
      SCHED_MUQSS = yes;
    };
    ignoreConfigErrors = true;
  });

  mpv-with-vapoursynth = with super; wrapMpv (pkgs.mpv-unwrapped.override {
    vapoursynthSupport = true;
  }) {
    extraMakeWrapperArgs = [
      "--prefix" "LD_LIBRARY_PATH" ":" "${vapoursynth-mvtools}/lib/vapoursynth"
    ];
  };

  # Enable systemd patch to avoid killUserProcess issue
  tmux-with-systemd = with super; tmux.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [
      (fetchpatch {
        url = "https://github.com/tmux/tmux/files/4575147/tmux-systemd.diff.txt";
        sha256 = "08kmk28pz2z8hb46z864f01q3b65w7x3ax3kbk9qrqv93rpr93hy";
      })
    ];
    buildInputs = (oldAttrs.buildInputs or []) ++ [
      super.systemd
    ];
    configureFlags = (oldAttrs.configureFlags or []) ++ [
      "--enable-systemd"
    ];
  });

  # Fixed backport to use nixpkgs-unstable packages as pkgs.unstable.<package>
  unstable = import unstableTarball {
    config = super.config;
  };

  # Backport from unstable to have Python 3 version
  cpuset-with-patch = with unstable; cpuset.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [
      (fetchpatch {
        url = "https://github.com/lpechacek/cpuset/files/5792001/cpuset2.txt";
        sha256 = "0rrgfixznhyymahakz31i396nj26qx9mcdavhm5cpkcfiqmk8nzl";
      })
    ];
  });

  # Backport for newer version used in services.picom
  picom = unstable.picom;
}
