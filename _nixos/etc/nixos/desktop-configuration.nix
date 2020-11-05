{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    arandr
    calibre
    chromium
    desktop-file-utils
    firefox
    gimp
    gnome3.baobab
    gnome3.evince
    gnome3.gnome-disk-utility
    gnome3.gnome-themes-standard
    gthumb
    inkscape
    insync
    keepassx-community
    kitty
    libreoffice-fresh
    lxappearance
    lxmenu-data
    pavucontrol
    pcmanfm
    peek
    perlPackages.FileMimeInfo
    qalculate-gtk
    redshift
    shared-mime-info
    smartmontools
    termite
    unstable.discord
    unstable.spotify
    unstable.tdesktop
    xarchiver
    xdotool
    xorg.xdpyinfo
    xorg.xhost
    xorg.xkill
    xorg.xset
  ];

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

  hardware = {
    # Enable sound.
    pulseaudio = {
      enable = true;
      extraConfig = ''
        # Switch between headset and headphone mode (e.g. for calls and music) automatically
        load-module module-bluetooth-policy auto_switch=2
        # Echo cancellation and noise cleanup of mic
        load-module module-echo-cancel aec_method=webrtc
      '';
    };
  };

  # Enable Java.
  programs.java = {
    enable = true;
    package = pkgs.jdk11;
  };

  # Enable Java anti-aliasing.
  environment.variables._JAVA_OPTIONS = "-Dswing.aatext=TRUE -Dawt.useSystemAAFontSettings=on";

  # Enable Gnome Keyring
  security.pam.services.gdm.enableGnomeKeyring = true;
  services.gnome3.gnome-keyring.enable = true;

  # Enable SMART monitoring.
  services.smartd = {
    enable = true;
    notifications.x11.enable = true;
  };
}
