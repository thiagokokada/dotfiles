{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

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


  # Add user to libvirtd group.
  users.users.thiagoko = {
     extraGroups = [ "libvirtd" ];
  };

  # Some misc packages.
  environment.systemPackages = with pkgs; [
    btrfs-progs
    virtmanager
  ];

  # Enable Plex Media Server.
  services.plex = {
    enable = true;
    openFirewall = true;
  };

  systemd.user.services.rtorrent-with-tmux = {
    description = "rtorrent: An ncurses client for libtorrent, running inside tmux";
    after = [ "network.target" ];
    wantedBy = [ "default.target" ];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.tmux}/bin/tmux -2 new-session -d -s rtorrent ${pkgs.rtorrent}/bin/rtorrent";
      ExecStop = "${pkgs.procps}/bin/pkill -SIGINT rtorrent";
      RemainAfterExit = "yes";
    };
  };

  systemd.user.services.rtorrent-update-ipv4-blocklist = {
    description = "Update IPv4 blocklist for rtorrent";
    after = [ "network.target" ];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.curl}/bin/curl https://silo.glasz.org/antip2p.list.gz' | ${pkgs.gzip} > ~/.session/antip2p.list";
    };
  };

  systemd.user.timers.rtorrent-update-ipv4-blocklist = {
    description = "Update IPv4 blocklist for rtorrent daily";
    wantedBy = [ "timers.target" ];

    timerConfig = {
      OnCalendar = "daily";
      Persistent = "true";
    };
  };

  # Reduce latency.
  powerManagement.cpuFreqGovernor = "performance";
}
