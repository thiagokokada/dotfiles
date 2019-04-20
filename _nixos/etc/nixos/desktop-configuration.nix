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
    "intel_iommu=on"
    "vfio-pci.ids=10de:1c02,10de:10f1"
  ];

  # Enable libvirtd.
  virtualisation = {
    libvirtd = {
      enable = true;
      onShutdown = "shutdown";
      qemuOvmf = true;
      qemuVerbatimConfig = ''
        nographics_allow_host_audio = 1
        cgroup_device_acl = [
          "/dev/null", "/dev/full", "/dev/zero",
          "/dev/random", "/dev/urandom",
          "/dev/ptmx", "/dev/kvm", "/dev/kqemu",
          "/dev/rtc","/dev/hpet",
          "/dev/input/by-id/usb-Logitech_G203_Prodigy_Gaming_Mouse_0176375A3336-event-mouse",
          "/dev/input/by-id/usb-Logitech_Gaming_Keyboard_G610_106936603934-event-kbd"
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
    piper
    samba
    smartmontools
    virtmanager
  ];

  services = {
    # Enable Plex Media Server.
    plex = {
      enable = true;
      openFirewall = true;
    };

    ratbagd = {
      enable = true;
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
        hosts allow = 192.168.15. localhost
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
  systemd.services.rtorrent-with-tmux = {
    description = "rtorrent: An ncurses client for libtorrent, running inside tmux";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "oneshot";
      User = "thiagoko";
      ExecStart = "${pkgs.tmux}/bin/tmux -2 new-session -d -s rtorrent ${pkgs.rtorrent}/bin/rtorrent";
      ExecStop = "${pkgs.procps}/bin/pkill -SIGINT rtorrent";
      RemainAfterExit = "yes";
    };
  };

  systemd.services.rtorrent-update-ipv4-blocklist = {
    description = "Update IPv4 blocklist for rtorrent";
    after = [ "network.target" ];

    serviceConfig = {
      Type = "oneshot";
      User = "thiagoko";
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

  # Install Java.
  programs.java.enable = true;
  environment.variables._JAVA_OPTIONS = "-Dswing.aatext=TRUE -Dawt.useSystemAAFontSettings=on";

  # Reduce latency.
  powerManagement.cpuFreqGovernor = "performance";
}
