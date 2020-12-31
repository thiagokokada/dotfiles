{ config, pkgs, ... }:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz;
  unstablePkgs = import unstableTarball {
    config = config.nixpkgs.config;
  };
in {
  # Backport module from unstable.
  imports = [ "${unstableTarball}/nixos/modules/hardware/opentabletdriver.nix" ];

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    overlays = [
      (self: super: {
        unstable = unstablePkgs;
        # Make a special opentabletdriver exception since it is needed by module above.
        opentabletdriver = unstablePkgs.opentabletdriver;
      })
    ];
  };

  boot = {
    # Mount /tmp using tmpfs for performance.
    tmpOnTmpfs = true;

    # Enable NTFS support.
    supportedFilesystems = [ "ntfs" ];

    kernel.sysctl = {
      # Enable Magic keys.
      "kernel.sysrq" = 1;
      # Reduce swap preference.
      "vm.swappiness" = 10;
    };
  };

  # Change some default locales.
  environment.variables = {
    LC_CTYPE = "pt_BR.UTF-8"; # Fix ç in us-intl.
    LC_TIME = "pt_BR.UTF-8";
    LC_COLLATE = "C"; # Use C style string sort.
  };

  # Increase file handler limit.
  security.pam.loginLimits = [{
    domain = "*";
    type = "hard";
    item = "nofile";
    value = "1048576";
  }];

  # Configure hardware for Intel.
  hardware = {
    # Enable CPU microcode for Intel.
    cpu.intel.updateMicrocode = true;
  };

  # Enable automatic GC.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # Enable NixOS auto-upgrade.
  system.autoUpgrade = {
    enable = true;
    dates = "daily";
  };

  services = {
    # Kill process consuming too much memory before it crawls the machine.
    earlyoom.enable = true;

    # Trim SSD weekly.
    fstrim = {
      enable = true;
      interval = "weekly";
    };

    # Decrease journal size.
    journald.extraConfig = ''
      SystemMaxUse=500M
    '';

    # Enable NTP.
    timesyncd.enable = true;
  };

  # Enable zram to have better memory management.
  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };
}
