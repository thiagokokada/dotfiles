{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  networking = {
    # Use wpa_supplicant.
    # wireless.enable = true;

    # Use Network Manager.
    networkmanager.enable = true;
  };

  # Start nm-applet.
  systemd.user.services.nm-applet = {
    description = "Network manager applet";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
  };

  boot = {
    # Mount /tmp using tmpfs for performance.
    tmpOnTmpfs = true;

    # Enable blk-mq.
    kernelParams = [
      "scsi_mod.use_blk_mq=1"
    ];
  };

  # Install laptop related packages.
  environment.systemPackages = with pkgs; [
    blueman
    glxinfo
    linuxPackages.cpupower
    lshw
    pciutils
    powertop
    psmisc
  ];

  # Configure special hardware in laptops.
  hardware = {
    # Enable bluetooth.
    bluetooth.enable = true;

    # Enable bumblebee to dynamic switch Intel/NVIDIA GPUs.
    bumblebee = {
      enable = true;
      pmMethod = "bbswitch";
    };

    # Enable CPU microcode for Intel.
    cpu.intel.updateMicrocode = true;

    # Extra OpenGL options.
    opengl = {
      extraPackages = with pkgs; [
        libvdpau-va-gl
        vaapiIntel
        vaapiVdpau
      ];
    };
  };

  # Enable laptop specific services.
  services = {
    # Enable natural scrolling.
    xserver = {
      libinput = {
        enable = true;
        naturalScrolling = true;
      };

      # Default Intel drivers are terrible.
      videoDrivers = [ "modesetting" ];
    };

    # Trim SSD weekly.
    fstrim = {
      enable = true;
      interval = "weekly";
    };

    # Enable NTP.
    timesyncd.enable = true;

    # Enable TLP to reduce power consumption
    tlp = {
      enable = true;
      extraConfig = ''
        # Enable powersave governor
        CPU_SCALING_GOVERNOR_ON_AC=powersave
        CPU_SCALING_GOVERNOR_ON_BAT=powersave
      '';
    };

    # Set scheduler depending on disk type.
    udev.extraRules = ''
      ACTION=="add|change", KERNEL=="[sv]d[a-z]", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
      ACTION=="add|change", KERNEL=="[sv]d[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    '';
  };
}
