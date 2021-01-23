{ config, lib, pkgs, ... }:

{
  programs.mako = with config.my.theme.colors; {
    enable = true;
    font = with config.my.fonts; "${gui.name} 12";
    backgroundColor = base00;
    textColor = base05;
    width = 200;
    borderSize = 1;
    borderColor = base01;
    defaultTimeout = 10000;
    padding = "8";
  };

  systemd.user.services.mako = {
    Unit = {
      Description = "Lightweight Wayland notification daemon";
      Documentation = "man:mako(1)";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.mako}/bin/mako";
      ExecReload = "${pkgs.mako}/bin/makoctl reload";
    };

    Install = { WantedBy = [ "graphical-session.target" ]; };
  };
}
