{ pkgs, ... }:

{
  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  # Use Network Manager.
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;

  # Enable blk-mq.
  boot.kernelParams = [
    "scsi_mod.use_blk_mq=1"
  ];

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

    # Enable bumblebee to turn down NVIDIA card.
    bumblebee.enable = false;

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

    # NVIDIA binary-blob settings
    nvidia = {
      optimus_prime = {
        enable = true;
        nvidiaBusId = "PCI:1:0:0";
        intelBusId = "PCI:0:2:0";
      };
      modesetting.enable = true;
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

      # Enable NVIDIA drivers.
      videoDrivers = [ "nvidia" ];
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
