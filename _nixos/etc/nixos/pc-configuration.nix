{ pkgs, config, lib, ... }:

let
  user = "thiagoko";
  group = "users";
  homePath = lib.strings.concatStrings [ "/home/" user ];
  archivePath = lib.strings.concatStrings [ "/mnt/archive/" user ];
in {
  boot = {
    # Early load i195 for better resolution in init.
    initrd.kernelModules = [ "i915" ];

    # Do not load NVIDIA drivers.
    blacklistedKernelModules = [ "nvidia" "nouveau" ];

    # Load VFIO related modules.
    kernelModules = [ "vfio_virqfd" "vfio_pci" "vfio_iommu_type1" "vfio" ];
    extraModprobeConfig = "options vfio-pci ids=10de:1c02,10de:10f1";

    # Enable IOMMU
    kernelParams = [
      "intel_iommu=on"
    ];
  };

  # Enable libvirtd.
  virtualisation = {
    libvirtd = {
      enable = true;
      qemuOvmf = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      # Create symbolic links for /var/lib/libvirt/inputs/event-mouse and
      # /var/lib/libvirt/inputs/event-kbd by probing the entries in either
      # /dev/input/by-id or /dev/input/by-path.
      qemuVerbatimConfig = ''
        nographics_allow_host_audio = 1
        cgroup_device_acl = [
          "/dev/null", "/dev/full", "/dev/zero",
          "/dev/random", "/dev/urandom",
          "/dev/ptmx", "/dev/kvm", "/dev/kqemu",
          "/dev/rtc","/dev/hpet",
          "/var/lib/libvirt/inputs/event-mouse",
          "/var/lib/libvirt/inputs/event-kbd"
        ]
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
    cpuset
    hdparm
    rtorrent
    samba
    virtmanager
  ];

  services = {
    # Enable btrfs scrub.
    btrfs.autoScrub = {
      enable = true;
      interval = "weekly";
    };

    # Enable irqbalance service.
    irqbalance.enable = true;

    # Enable Plex Media Server.
    plex = {
      enable = true;
      openFirewall = true;
      group = group;
    };

    # Enable Samba.
    samba = {
      enable = true;
      package = pkgs.samba;
      extraConfig = ''
        workgroup = WORKGROUP
        local master = yes
        preferred master = yes
        server string = ${config.networking.hostName}
        netbios name = ${config.networking.hostName}
        use sendfile = yes
        hosts allow = 192.168.0.0/16 172.16.0.0/12 10.0.0.0/8 localhost
        hosts deny = 0.0.0.0/0
        guest account = nobody
        map to guest = bad user
        mangled names = no
        vfs objects = catia
        catia:mappings = 0x22:0xa8,0x2a:0xa4,0x2f:0xf8,0x3a:0xf7,0x3c:0xab,0x3e:0xbb,0x3f:0xbf,0x5c:0xff,0x7c:0xa6
      '';
      shares = {
        home = {
          path = homePath;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = user;
          "force group" = group;
        };
        archive = {
          path = archivePath;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = user;
          "force group" = group;
        };
      };
    };

    # Enable rtorrent
    rtorrent = {
      enable = true;
      downloadDir =  lib.strings.concatStrings [ archivePath "/Downloads" ];
      user = user;
      group = group;
      port = 60001;
      openFirewall = true;
      configText = ''
        # Enable the default ratio group.
        ratio.enable=

        # Change the limits, the defaults should be sufficient.
        ratio.min.set=100
        ratio.max.set=300
        ratio.upload.set=500M

        # Watch directory
        schedule2 = watch_directory,5,5,load.start="${homePath}/Torrents/*.torrent"
        schedule2 = untied_directory,5,5,stop_untied=
      '';
    };

     # Enable support for tablets
     xserver.digimend.enable = true;
  };

  systemd.services.flood = {
    description = "A web UI for rTorrent with a Node.js backend and React frontend.";
    after = [ "rtorrent.service" ];
    path = [ pkgs.unstable.nodePackages.flood pkgs.bash ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = user;
      Group = group;
      Type = "simple";
      Restart = "on-failure";
      ExecStart="${pkgs.unstable.nodePackages.flood}/bin/flood";
    };
  };

  networking = {
    # Needs to disable global DHCP to use bridge interfaces
    useDHCP = false;
    interfaces.br0.useDHCP = true;

    # Enable bridge.
    bridges = {
      br0 = {
        interfaces = [ "eno1" ];
      };
    };

    # Open ports to Samba.
    firewall = {
      allowedTCPPorts = [ 139 445 ];
      allowedUDPPorts = [ 137 138 ];
    };
  };

  # Reduce latency.
  powerManagement.cpuFreqGovernor = "performance";
}
