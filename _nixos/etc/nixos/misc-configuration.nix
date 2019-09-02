{ pkgs, ... }:

{
  boot = {
    # Mount /tmp using tmpfs for performance.
    tmpOnTmpfs = true;

    kernelParams = [
      # Enable blk-mq.
      "scsi_mod.use_blk_mq=1"
    ];

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

    # Suspend when power key is pressed
    logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    # Enable Intel Thermald.
    thermald.enable = true;

    # Enable NTP.
    timesyncd.enable = true;

    # Set blk-mq scheduler depending on disk type.
    udev.extraRules = ''
      # set scheduler for SSD
      ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*|nvme[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
      # set scheduler for rotating disks
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    '';
  };
}
