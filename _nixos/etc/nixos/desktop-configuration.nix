{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    arandr
    bitwarden
    calibre
    chromium
    desktop-file-utils
    firefox
    gammastep
    gimp
    gnome3.baobab
    gnome3.evince
    gnome3.gnome-disk-utility
    gnome3.gnome-themes-standard
    gthumb
    inkscape
    kitty
    libreoffice-fresh
    lxappearance
    lxmenu-data
    pavucontrol
    pcmanfm
    peek
    perlPackages.FileMimeInfo
    qalculate-gtk
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

  # Enable Gnome Keyring
  security.pam.services.gdm.enableGnomeKeyring = true;
  services.gnome3.gnome-keyring.enable = true;

  # Enable SMART monitoring.
  services.smartd = {
    enable = true;
    notifications.x11.enable = true;
  };
}
