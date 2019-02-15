{ pkgs, config, ... }:

let
  unstable = import (fetchGit {
    name = "nixos-unstable-2019-01-10";
    url = https://github.com/nixos/nixpkgs/;
    rev = "77ec7d75a9effb1d45dc3c01fb1261be43bcc1e0"; # py3status 3.15
  }) {
    config = config.nixpkgs.config;
  };
in
{
  environment = {
    variables = {
      # Export modules to allow PCManFM to use gvfs.
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
    };

    # Desktop packages.
    systemPackages = with pkgs; [
      arandr
      arc-icon-theme
      arc-theme
      chromium
      dropbox-cli
      ffmpegthumbnailer
      firefox
      gimp
      gnome3.adwaita-icon-theme
      gnome3.baobab
      gnome3.evince
      gnome3.file-roller
      gnome3.gnome-disk-utility
      gnome3.gnome-themes-standard
      gthumb
      gtk-engine-murrine
      hicolor-icon-theme
      inkscape
      keepassx-community
      libreoffice-fresh
      lxmenu-data
      pcmanfm
      peek
      perlPackages.FileMimeInfo
      qalculate-gtk
      shared_mime_info
      termite
      unstable.kitty
    ];
  };

  # Added fonts used by i3.
  fonts = {
    enableDefaultFonts = true;
    enableFontDir = true;

    fonts = with pkgs; [
      cantarell-fonts
      corefonts
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

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  services = {
    # Setup DBus.
    dbus.packages = [ pkgs.gnome3.dconf ];
    # Allow automounting.
    gnome3.gvfs.enable = true;

    xserver = {
      enable = true;

      # Use LightDM.
      displayManager = with pkgs; {
        lightdm = {
          enable = true;
          greeters = {
            gtk = {
              enable = true;
              clock-format = "%a %d/%m %H:%M:%S";
              iconTheme = {
                package = arc-icon-theme;
                name = "Arc";
              };
              indicators = [ "~clock" "~session" "~power" ];
              theme = {
                package = arc-theme;
                name = "Arc-Dark";
              };
            };
          };
        };
      };

      # Disable Xterm.
      desktopManager = {
        xterm.enable = false;
        default = "none";
      };

      # Configure i3-gaps as WM.
      windowManager = {
        default = "i3";
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          # i3 dependencies.
          extraPackages = with pkgs; [
            (with unstable.python3Packages;
              (py3status.overrideAttrs (oldAttrs: {
                propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
                  i3ipc
                  pydbus
                  pygobject3
                ];
              })))
            (with unstable;
              (ranger.overrideAttrs (oldAttrs: {
                propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
                  python3Packages.pillow
                ];
              })))
            compton-git
            dex
            dunst
            ffmpeg
            i3lock
            i3status
            iw
            libnotify
            lm_sensors
            maim
            nitrogen
            pavucontrol
            playerctl
            redshift
            rofi
            xcape
            xdg-user-dirs
            xorg.xdpyinfo
            xorg.xkill
            xorg.xset
            xsettingsd
            xss-lock
            wmctrl
          ];
        };
      };

      # Remap Caps Lock to Esc
      xkbOptions = "ctrl:nocaps";
    };
  };

  # Configure special programs (i.e. hardware access).
  programs = {
    # Enable dconf.
    dconf.enable = true;

    # Backlight control.
    light.enable = true;

    # Enable Qt5 integration.
    qt5ct.enable = true;
  };
}
