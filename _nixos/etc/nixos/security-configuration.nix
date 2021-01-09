{ pkgs, ... }:

let
  # Mostly safe to block unless the service is doing something very strange
  safeHardeningFlags = {
    LockPersonality = true;
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectHostname = true;
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
    ProtectKernelLogs = true;
    ProtectKernelModules = true;
    ProtectKernelTunables = true;
  };
  restrictNetworkFlags = {
    PrivateNetwork = true; # Doesn't work in user services
    RestrictAddressFamilies="AF_UNIX";
  };
in {
  # systemd-analyze --user security
  systemd.user.services = {
    geoclue-agent.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    opentabletdriver.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    picom.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    redshift.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
    xsettingsd.serviceConfig = safeHardeningFlags // restrictNetworkFlags;
  };

  # systemd-analyze security
  systemd.services = {
    flood.serviceConfig = strictHardeningFlags // { ProtectHome = false; };
    rtorrent.serviceConfig = strictHardeningFlags;
    plex.serviceConfig = strictHardeningFlags;
    samba-nmbd.serviceConfig = safeHardeningFlags;
    samba-smbd.serviceConfig = safeHardeningFlags;
    samba-winbindd.serviceConfig = safeHardeningFlags;
    smartd.serviceConfig = strictHardeningFlags // restrictNetworkFlags // { ProtectClock = false; PrivateDevices = false; };
  };

  # TODO: Enable usbguard after finding some way to easily manage it
  # services.usbguard = {
  #   enable = true;
  #   presentDevicePolicy = "keep";
  # };
}
