{ pkgs, ... }:

let
  # Mostly safe to block unless the service is doing something very strange
  safeHardeningFlags = {
    LockPersonality = true;
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectSystem = true;
    RestrictNamespaces = true;
    RestrictRealtime = true;
    SystemCallArchitectures = "native";
  };
  strictHardeningFlags = safeHardeningFlags // {
    PrivateControlGroups = true;
    PrivateDevices = true;
    ProtectClock = true;
    ProtectHome = true;
    ProtectHostname = true;
    ProtectKernelLogs = true;
    ProtectKernelModules = true;
    ProtectKernelTunables = true;
  };
  restrictNetworkFlags = {
    RestrictAddressFamilies="AF_UNIX";
  };
in {
  # systemd-analyze --user security
  systemd.user.services = {
    opentabletdriver.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    picom.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    redshift.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    xsettingsd.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
  };

  # systemd-analyze security
  systemd.services = {
    flood.serviceConfig = strictHardeningFlags // restrictNetworkFlags // { ProtectHome = false; };
    rtorrent.serviceConfig = strictHardeningFlags // { ProtectHome = "read-only"; };
    plex.serviceConfig = strictHardeningFlags;
    samba-nmbd.serviceConfig = safeHardeningFlags;
    samba-smbd.serviceConfig = safeHardeningFlags;
    samba-winbindd.serviceConfig = safeHardeningFlags;
    smartd.serviceConfig = strictHardeningFlags // { ProtectClock = false; PrivateDevices = false; PrivateNetwork = true; };
  };

  # TODO: Enable usbguard after finding some way to easily manage it
  # services.usbguard = {
  #   enable = true;
  #   presentDevicePolicy = "keep";
  # };
}
