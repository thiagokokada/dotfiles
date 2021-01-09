{ pkgs, ... }:

let
  minimalServicesHardenedFlags = {
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectHostname = true;
    ProtectSystem = "full";
    RestrictNamespaces = true;
    RestrictRealtime = true;
    RestrictSUIDSGID = true;
  };
  userServicesHardenedFlags = minimalServicesHardenedFlags // {
    ProtectHome = "read-only";
    ProtectSystem = "strict";
    RestrictAddressFamilies="AF_UNIX AF_INET AF_INET6";
  };
  servicesHardenedFlags = userServicesHardenedFlags // {
    PrivateControlGroups = true;
    PrivateDevices = true;
    ProtectHome = true;
    ProtectKernelLogs = true;
    ProtectKernelModules = true;
    ProtectKernelTunables = true;
  };
in {
  # systemd-analyze --user security
  systemd.user.services = {
    geoclue-agent.serviceConfig = userServicesHardenedFlags;
    opentabletdriver.serviceConfig = userServicesHardenedFlags // { ProtectHome = false; };
    picom.serviceConfig = userServicesHardenedFlags;
    redshift.serviceConfig = userServicesHardenedFlags;
    xsettingsd.serviceConfig = userServicesHardenedFlags;
  };

  # systemd-analyze security
  systemd.services = {
    flood.serviceConfig = servicesHardenedFlags // { ProtectHome = false; };
    rtorrent.serviceConfig = servicesHardenedFlags // { ProtectSystem = "full"; };
    plex.serviceConfig = servicesHardenedFlags // { ProtectSystem = "full"; };
    samba-nmbd.serviceConfig = minimalServicesHardenedFlags;
    samba-smbd.serviceConfig = minimalServicesHardenedFlags;
    samba-winbindd.serviceConfig = minimalServicesHardenedFlags;
    smartd.serviceConfig = servicesHardenedFlags // { PrivateDevices = false; PrivateNetwork = true; };
  };

  # TODO: Enable usbguard after finding some way to easily manage it
  # services.usbguard = {
  #   enable = true;
  #   presentDevicePolicy = "keep";
  # };
}
