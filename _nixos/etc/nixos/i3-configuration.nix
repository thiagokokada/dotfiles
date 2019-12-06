{ pkgs, ... }:

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
      insync
      keepassx-community
      kitty
      libreoffice-fresh
      lxappearance-gtk3
      lxmenu-data
      mcomix
      pcmanfm
      peek
      perlPackages.FileMimeInfo
      qalculate-gtk
      qt5.qttools
      ranger
      redshift
      shared-mime-info
      spotify
      termite
      xorg.xf86inputlibinput
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

  hardware ={
    # Enable sound.
    pulseaudio.enable = true;
  };

  services = {
    # Allow automounting.
    gvfs.enable = true;

    udev.packages = [ pkgs.libinput.out ];

    xserver = {
      enable = true;

      modules = [ pkgs.xorg.xf86inputlibinput ];

      # Set extra config to libinput devices
      inputClassSections = [
        ''
          Identifier "mouse"
          Driver "libinput"
          MatchIsPointer "on"
          Option "AccelProfile" "flat"
        ''
        ''
          Identifier "touchpad"
          Driver "libinput"
          MatchIsTouchpad "on"
          Option "NaturalScrolling" "true"
        ''
      ];

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

      # Configure i3-gaps as WM.
      windowManager = {
        default = "i3";
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          # i3 dependencies.
          extraPackages = with pkgs; [
            compton-git
            dex
            dmenu
            dunst
            ffmpeg
            i3lock
            i3status-rust
            libnotify
            lm_sensors
            maim
            nitrogen
            pavucontrol
            playerctl
            rofi
            wmctrl
            xdg-user-dirs
            xkblayout-state
            xorg.xdpyinfo
            xorg.xkill
            xorg.xset
            xsecurelock
            xss-lock
          ];
        };
      };

      # Remap Caps Lock to Esc
      xkbOptions = "caps:escape";
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
