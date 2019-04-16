{ pkgs, ... }:

{
  hardware = {
    # Enable NVIDIA in exclusive mode.
    # nvidia = {
    #   optimus_prime = {
    #     enable = true;
    #     # Bus ID of the NVIDIA GPU. You can find it using lspci
    #     nvidiaBusId = "PCI:1:0:0";
    #     # Bus ID of the Intel GPU. You can find it using lspci
    #     intelBusId = "PCI:0:2:0";
    #   };
    #   # Use KMS with NVIDIA drivers.
    #   modesetting.enable = true;
    # };

    # Enable bumblebee to dynamic switch Intel/NVIDIA GPUs.
    bumblebee = {
      enable = true;
      pmMethod = "bbswitch";
    };
  };

  # Workaround Bumblebee issue.
  # https://github.com/Bumblebee-Project/Bumblebee/issues/971#issuecomment-410386426
  # environment.variables.__GLVND_DISALLOW_PATCHING = "1";

  # Enable laptop specific services.
  services = {
    xserver = {
      # Use NVIDIA driver.
      # videoDrivers = [ "nvidia" ];
    };
  };
}
