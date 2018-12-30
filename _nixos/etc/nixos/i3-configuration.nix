{ pkgs, ... }:

{
  # Install i3 related packages.
  environment.systemPackages = with pkgs; [
    (python36.withPackages(ps: with ps; [ i3ipc pydbus pygobject3 py3status ]))
    arc-icon-theme
    arc-theme
    compton
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
    nitrogen
    playerctl
    ranger
    redshift
    rofi
    stow
    termite
    xclip
    xdg-user-dirs
    xorg.xdpyinfo
    xorg.xinit
    xorg.xkill
    xorg.xset
    xss-lock
  ];

  # Added fonts used by i3.
  fonts = {
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      font-awesome_5
      hack-font
      inconsolata
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      powerline-fonts
      roboto
      source-code-pro
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
  };

  # Enable dconf service.
  services.dbus.packages = [ pkgs.gnome3.dconf ];
}
