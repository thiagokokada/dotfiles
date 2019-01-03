{ pkgs, ... }:

{
  # Install i3 related packages.
  environment.systemPackages = with pkgs; [
    (python3Packages.py3status.overrideAttrs (oldAttrs: {
      propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
        python3Packages.i3ipc
        python3Packages.pydbus
        python3Packages.pygobject3
      ];
    }))
    arc-icon-theme
    arc-theme
    chromium
    compton-git
    dropbox-cli
    dunst
    ffmpeg
    ffmpegthumbnailer
    firefox
    gnome3.file-roller
    gnome3.gnome-themes-standard
    gtk-engine-murrine
    iw
    keepassx-community
    kitty
    libnotify
    lm_sensors
    maim
    mpv-with-scripts
    nitrogen
    playerctl
    qalculate-gtk
    ranger
    redshift
    rofi
    stow
    termite
    xdg-user-dirs
    xorg.xdpyinfo
    xorg.xinit
    xorg.xkill
    xorg.xset
    xss-lock
    zathura
  ];

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

  # Enable i3+xfce.
  services.xserver = {
    enable = true;

    # Use LightDM as DM.
    displayManager = {
      lightdm.enable = true;
    };

    # Use XFCE as base DE.
    desktopManager = {
      default = "xfce";
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
        thunarPlugins = [ pkgs.xfce.thunar-archive-plugin
                          pkgs.xfce.thunar-dropbox-plugin
                          pkgs.xfce.thunar-volman ];
      };
    };

    # Configure i3-gaps as WM.
    windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
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

  # Enable dconf service.
  services.dbus.packages = [ pkgs.gnome3.dconf ];
}
