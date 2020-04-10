{ pkgs, config, ... }:

{
  nixpkgs.config.allowUnfree = true;

  # Early load i195 for better resolution in init.
  boot.initrd.kernelModules = [ "i915" ];

  # Do not load NVIDIA drivers.
  boot.blacklistedKernelModules = [ "nvidia" "nouveau" ];

  # Load VFIO related modules.
  boot.kernelModules = [ "vfio_virqfd" "vfio_pci" "vfio_iommu_type1" "vfio" ];
  boot.extraModprobeConfig = "options vfio-pci ids=10de:1c02,10de:10f1";

  # Enable IOMMU
  boot.kernelParams = [
    "intel_iommu=on"
  ];

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
    hdparm
    rtorrent
    samba
    smartmontools
    virtmanager
  ];

  services = {
    # Enable btrfs scrub.
    btrfs.autoScrub = {
      enable = true;
      interval = "weekly";
    };

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

    # Enable SMART monitoring.
    smartd = {
      enable = true;
      notifications.x11.enable = true;
    };
  };

  networking = {
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

  # rtorrent daemon.
  systemd.user.services.rtorrent-with-tmux = {
    description = "rtorrent: An ncurses client for libtorrent, running inside tmux";
    after = [ "network.target" ];
    wantedBy = [ "default.target" ];
    environment = {
      HOME = "%h";
      TERMINFO = "${pkgs.kitty}/lib/kitty/terminfo";
    };
    path = [ pkgs.bash pkgs.tmux pkgs.procps ];

    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.tmux}/bin/tmux -2 -L rtorrent new-session -d -s rtorrent ${pkgs.rtorrent}/bin/rtorrent";
      ExecStop = "${pkgs.procps}/bin/pkill rtorrent";
      Restart = "on-failure";
    };
  };

  systemd.user.services.rtorrent-update-ipv4-blocklist = {
    description = "Update IPv4 blocklist for rtorrent";
    after = [ "network.target" ];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.curl}/bin/curl https://silo.glasz.org/antip2p.list.gz' | ${pkgs.gzip}/bin/gunzip > ~/.session/antip2p.list";
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
