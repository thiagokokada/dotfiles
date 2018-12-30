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
    kde-gtk-config
    kitty
    libnotify
    lm_sensors
    maim
    mpv-with-scripts
    nitrogen
    playerctl
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
      corefonts
      dejavu_fonts
      font-awesome_5
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      roboto
      ttf_bitstream_vera
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
    layout = "br";

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
    dconf.enable = true;
    light.enable = true;

    firejail = {
      enable = true;
      wrappedBinaries = {
        firefox = "${pkgs.firefox}/bin/firefox";
        mpv = "${pkgs.mpv}/bin/mpv";
      };
    };
  };

  # Enable dconf service.
  services.dbus.packages = [ pkgs.gnome3.dconf ];
}
