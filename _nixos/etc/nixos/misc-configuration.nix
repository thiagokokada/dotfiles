{ config, pkgs, lib, ... }:

let
  unstableTarball = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz";
  libinputModuleRefactor = fetchTarball {
    url = "https://github.com/thiagokokada/nixpkgs/archive/ee96caebbc51c6620c2f318f76fd848a4a5623a5.tar.gz";
    sha256 = "1mfpyf7d8d50300z1b523bqaql60q3yyszps176aqs8y04m1hjdr";
  };
  redshiftModuleRefactor = fetchTarball {
    url = "https://github.com/thiagokokada/nixpkgs/archive/36156bab123083ca422bc3f00e70ca4e00276df2.tar.gz";
    sha256 = "1mdwxswzks7i73pjz8yyzzr6ks9rlhb7kq2x5hx0d8mfrk66gyfn";
  };
in {
  # Backport module from unstable.
  imports = [
    "${unstableTarball}/nixos/modules/hardware/opentabletdriver.nix"
    "${unstableTarball}/nixos/modules/services/x11/picom.nix"
    "${libinputModuleRefactor}/nixos/modules/services/x11/hardware/libinput.nix"
    "${redshiftModuleRefactor}/nixos/modules/services/x11/redshift.nix"
  ];

  disabledModules = [
    "hardware/opentabletdriver.nix"
    "services/x11/hardware/libinput.nix"
    "services/x11/picom.nix"
    "services/x11/redshift.nix"
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    overlays = [ (import ./overlays) ];
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

    # Set I/O scheduler.
    udev.extraRules = ''
      # set scheduler for NVMe
      ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="mq-deadline"
      # set scheduler for SSD and eMMC
      ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="bfq"
      # set scheduler for rotating disks
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    '';
  };

  # Enable zram to have better memory management.
  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };
}
