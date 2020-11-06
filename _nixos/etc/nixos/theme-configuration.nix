{ pkgs, ... }:

let
  gtk-theme = "Arc-Dark";
  icon-theme = "Arc";
  cursor-theme = "Adwaita";
  fallback-theme = "gnome";
  font-name = "Noto Sans 11";
in {
  environment = {
    systemPackages = with pkgs; [
      arc-icon-theme
      arc-theme
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    etc."xdg/gtk-2.0/gtkrc" = {
      text = ''
        gtk-icon-theme-name = "${icon-theme}"
        gtk-theme-name = "${gtk-theme}"
        gtk-cursor-theme-name = "${cursor-theme}"
        gtk-fallback-icon-theme = "${fallback-theme}"
        gtk-font-name = "${font-name}"
      '';
      mode = "444";
    };
    etc."xdg/gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-icon-theme-name=${icon-theme}
        gtk-theme-name=${gtk-theme}
        gtk-cursor-theme-name=${cursor-theme}
        gtk-fallback-icon-theme=${fallback-theme}
        gtk-font-name=${font-name}
      '';
      mode = "444";
    };
  };

  systemd = {
    user.services = {
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
          Restart = "on-failure";
          ExecStart="${pkgs.xsettingsd}/bin/xsettingsd --config=${configFile}";
        };
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
    platformTheme = "gtk2";
    style = "gtk2";
  };
}
