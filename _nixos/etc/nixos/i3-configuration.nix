{ pkgs, ... }:

{
  environment = {
    variables = {
      # Export modules to allow PCManFM to use gvfs.
      GIO_EXTRA_MODULES = [ "${pkgs.gvfs}/lib/gio/modules" ];
    };

    # Desktop packages.
    systemPackages = with pkgs; [
      arc-icon-theme
      arc-theme
      chromium
      dropbox-cli
      ffmpeg
      ffmpegthumbnailer
      firefox
      gnome3.adwaita-icon-theme
      gnome3.evince
      gnome3.file-roller
      gnome3.gnome-themes-standard
      gthumb
      gtk-engine-murrine
      gvfs
      hicolor-icon-theme
      keepassx-community
      kitty
      lxappearance-gtk3
      lxmenu-data
      mpv-with-scripts
      networkmanagerapplet
      pcmanfm
      qalculate-gtk
      shared_mime_info
      termite
    ];
  };

  # Added fonts used by i3.
  fonts = {
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
      displayManager = {
        lightdm = {
          enable = true;
          greeters = {
            gtk = {
              enable = true;
              clock-format = "%a %d/%m %H:%M:%S";
              iconTheme = {
                package = pkgs.arc-icon-theme;
                name = "Arc";
              };
              indicators = [ "~clock" "~session" "~power" ];
              theme = {
                package = pkgs.arc-theme;
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
            (python3Packages.py3status.overrideAttrs (oldAttrs: {
              propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
                python3Packages.i3ipc
                python3Packages.pydbus
                python3Packages.pygobject3
              ];
            }))
            compton-git
            dunst
            i3lock
            i3status
            iw
            libnotify
            lm_sensors
            maim
            nitrogen
            playerctl
            ranger
            redshift
            rofi
            xdg-user-dirs
            xorg.xdpyinfo
            xorg.xkill
            xorg.xset
            xss-lock
          ];
        };
      };
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
