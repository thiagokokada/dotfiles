{ pkgs, ... }:

{
  # Early load i195 for better resolution in init.
  boot.initrd.kernelModules = [ "i915" ];

  # Do not load NVIDIA drivers.
  boot.blacklistedKernelModules = [ "nvidia" "nouveau" ];

  # Load VFIO related modules.
  boot.kernelModules = [ "vfio_virqfd" "vfio_pci" "vfio_iommu_type1" "vfio" ];

  # Enable IOMMU
  boot.kernelParams = [
    "pci=noaer"
    "intel_iommu=on"
    "vfio-pci.ids=10de:1c02,10de:10f1"
  ];

  # Enable libvirtd.
  virtualisation = {
    libvirtd = {
      enable = true;
      qemuOvmf = true;
      qemuVerbatimConfig = ''
        nographics_allow_host_audio = 1
      '';
    };
  };


  users.users.thiagoko = {
     extraGroups = [ "libvirtd" ];
  };

  environment.systemPackages = with pkgs; [
    btrfs-progs
    virtmanager
  ];

  # Reduce latency.
  powerManagement.cpuFreqGovernor = "performance";
}
