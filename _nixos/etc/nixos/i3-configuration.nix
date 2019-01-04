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
    (lxappearance.overrideAttrs(oldAttrs: rec {
      name = "lxappearance-0.6.2";
      src = fetchurl {
        url = "mirror://sourceforge/project/lxde/LXAppearance/${name}.tar.xz";
        sha256 = "07r0xbi6504zjnbpan7zrn7gi4j0kbsqqfpj8v2x94gr05p16qj4";
      };
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
    gnome3.evince
    gnome3.file-roller
    gnome3.gnome-themes-standard
    gthumb
    gtk-engine-murrine
    hicolor-icon-theme
    iw
    keepassx-community
    kitty
    libnotify
    lm_sensors
    lxmenu-data
    maim
    mpv-with-scripts
    networkmanagerapplet
    nitrogen
    pcmanfm
    playerctl
    qalculate-gtk
    ranger
    redshift
    rofi
    shared_mime_info
    stow
    termite
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
    dbus.packages = [ pkgs.gnome3.dconf ];
    devmon.enable = true;
    udisks2.enable = true;

    xserver = {
      enable = true;

      # Use LightDM as DM.
      displayManager = {
        lightdm.enable = true;
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
