# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Allow Unfree packages.
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [ "pci=noaer" ];

  networking.hostName = "mikudayo-nixos"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  boot.initrd.luks.devices = [
    {
      name = "enc-pv";
      device = "/dev/disk/by-uuid/1174d46f-5d5d-4a69-b17b-f4b5ee9b7598";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "br-abnt2";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    (python27Full.withPackages(ps: with ps; [ pydbus pygobject3 py3status pip requests tkinter virtualenv ]))
    (python36Full.withPackages(ps: with ps; [ pip requests tkinter virtualenv ]))
    arc-icon-theme
    arc-theme
    compton
    curl
    dunst
    ffmpeg
    ffmpegthumbnailer
    firefox
    gcc
    gitFull
    glxinfo
    gnome3.file-roller
    gnome3.gnome-themes-standard
    gnumake
    gtk-engine-murrine
    htop
    kde-gtk-config
    kitty
    libnotify
    lm_sensors
    lshw
    maim
    mpv
    neovim
    nitrogen
    pciutils
    psmisc
    playerctl
    ranger
    redshift
    rofi
    stow
    termite
    vim
    wget
    xdg-user-dirs
    xorg.xbacklight
    xorg.xdpyinfo
    xorg.xinit
    xorg.xkill
    xorg.xset
    xss-lock
  ];

  fonts.fonts = with pkgs; [
    cantarell-fonts
    corefonts
    dejavu_fonts
    font-awesome_4
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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "br";
    displayManager = {
      lightdm.enable = true;
    };
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
    windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
    };
  };

  # Nvidia binary-blob settings.
  # services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.nvidia.optimus_prime.enable = true;
  # Bus ID of the NVIDIA GPU. You can find it using lspci
  # hardware.nvidia.optimus_prime.nvidiaBusId = "PCI:1:0:0";
  # Bus ID of the Intel GPU. You can find it using lspci
  # hardware.nvidia.optimus_prime.intelBusId = "PCI:0:2:0";

  # Enable dconf.
  programs.dconf.enable = true;
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  # Enable zsh.
  programs.zsh.enable = true;

  # Enable touchpad support.
  services.xserver.libinput = {
    enable = true;
    naturalScrolling = true;
  };

  # Enable TLP to reduce energy consumption.
  services.tlp.enable = true;

  # Trim SSD periodically.
  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.thiagoko = {
     isNormalUser = true;
     uid = 1000;
     extraGroups = [ "wheel" "networkmanager" ];
     shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
