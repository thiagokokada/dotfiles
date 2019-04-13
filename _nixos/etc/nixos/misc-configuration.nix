{ pkgs, ... }:

{
  boot = {
    # Mount /tmp using tmpfs for performance.
    tmpOnTmpfs = true;

    # Enable blk-mq.
    kernelParams = [
      "scsi_mod.use_blk_mq=1"
    ];
  };

  # Change some default locales.
  environment.variables = {
    LC_CTYPE = "pt_BR.UTF-8"; # Fix ç in us-intl.
    LC_TIME = "pt_BR.UTF-8";
    LC_COLLATE = "C"; # Use C style string sort.
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
    # Trim SSD weekly.
    fstrim = {
      enable = true;
      interval = "weekly";
    };

    # Enable NTP.
    timesyncd.enable = true;

    # Set blk-mq scheduler depending on disk type.
    udev.extraRules = ''
      ACTION=="add|change", KERNEL=="[sv]d[a-z]", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="kyber"
      ACTION=="add|change", KERNEL=="[sv]d[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    '';
  };
}
