{ pkgs, lib, ... }:

let
  gtk-theme = "Adwaita-dark";
  icon-theme = "Adwaita";
  cursor-theme = "Adwaita";
  fallback-theme = "gnome";
  font-name = "Noto Sans 11";
  gtk-config = {
    gtk-icon-theme-name = icon-theme;
    gtk-theme-name = gtk-theme;
    gtk-cursor-theme-name = cursor-theme;
    gtk-fallback-icon-theme = fallback-theme;
    gtk-font-name = font-name;
  };
in {
  environment = {
    systemPackages = with pkgs; [
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];

    etc."xdg/gtk-2.0/gtkrc" = {
      text = lib.generators.toKeyValue {} gtk-config;
      mode = "444";
    };

    etc."xdg/gtk-3.0/settings.ini" = {
      text = lib.generators.toINI {} { Settings = gtk-config; };
      mode = "444";
    };
  };

  # Configure LightDM
  services.xserver.displayManager = {
    lightdm = {
      enable = true;
      greeters = {
        gtk = {
          enable = true;
          clock-format = "%a %d/%m %H:%M:%S";
          iconTheme = {
            name = "${icon-theme}";
          };
          indicators = [ "~clock" "~session" "~power" ];
          theme = {
            name = "${gtk-theme}";
          };
        };
      };
    };
  };

  systemd.user.services = {
    xsettingsd = let
      configFile = pkgs.writeText "xsettingsd" ''
        Net/IconThemeName "${icon-theme}"
        Net/ThemeName "${gtk-theme}"
        Gtk/CursorThemeName "${cursor-theme}"
      '';
    in {
      description = "Provides settings to X11 applications via the XSETTINGS specification";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart="${pkgs.xsettingsd}/bin/xsettingsd --config=${configFile}";
        RestartSec = 3;
        Restart = "on-failure";
      };
    };
  };

  fonts = {
    enableDefaultFonts = true;
    enableFontDir = true;

    fonts = with pkgs; [
      cantarell-fonts
      corefonts
      dejavu_fonts
      font-awesome_4
      font-awesome_5
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      roboto
      ttf_bitstream_vera
      ubuntu_font_family
    ];

    fontconfig = {
      dpi = 135;
      defaultFonts = {
        monospace = [ "Noto Mono" ];
        serif = [ "Noto Serif" ];
        sansSerif = [ "Noto Sans" ];
      };
    };
  };

  # Enable Qt5 integration.
  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };
}
