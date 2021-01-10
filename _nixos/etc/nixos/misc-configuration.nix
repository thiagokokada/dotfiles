{ config, pkgs, lib, ... }:

let
  unstableTarball = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz";
  libinputModuleRefactor = fetchTarball {
    url = "https://github.com/thiagokokada/nixpkgs/archive/ffc2d237b4a251486894816f5c45569f234d3a85.tar.gz";
    sha256 = "1w1j8hvibv2mmllggc9zg8p8vq1jg8yizapj3922k85br9jvmdlm";
  };
  redshiftModuleRefactor = fetchTarball {
    url = "https://github.com/thiagokokada/nixpkgs/archive/fab2665566f4cff50e13596c9b52e8550cf2309b.tar.gz";
    sha256 = "1avdj1d0frwhl38mzwwksm42bliqqrkw6rp7yfm8969anhzn2s5v";
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

    # Kills user process on exit instead of waiting them to finish.
    # Makes shutdown/reboot faster, but breaks tmux/screen
    logind.killUserProcesses = true;

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
