{ pkgs, config, ... }:

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
    samba
    virtmanager
  ];

  services = {
    # Enable Plex Media Server.
    plex = {
      enable = true;
      openFirewall = true;
    };

    # Enable Samba.
    samba = {
      enable = true;
      package = pkgs.samba;
      extraConfig = ''
        workgroup = WORKGROUP
        server string = ${config.networking.hostName}
        netbios name = ${config.networking.hostName}
        use sendfile = yes
        max protocol = smb2
        hosts allow = 192.168.15.0 192.168.122. localhost
        hosts deny = 0.0.0.0/0
        guest account = nobody
        map to guest = bad user
        mangled names = no
        vfs objects = catia
        catia:mappings = 0x22:0xf022, 0x2a:0xf02a, 0x2f:0xf02f, 0x3a:0xf03a, 0x3c:0xf03c, 0x3e:0xf03e, 0x3f:0xf03f, 0x5c:0xf05c, 0x7c:0xf07c, 0x20:0xf020
      '';
      shares = {
        home = {
          path = "/home/thiagoko";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "thiagoko";
          "force group" = "users";
        };
        archive = {
          path = "/mnt/archive/thiagoko";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "thiagoko";
          "force group" = "users";
        };
      };
    };
  };

  # Open ports to Samba.
  networking.firewall.allowedTCPPorts = [ 139 445 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];

  # rtorrent daemon.
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
