{ config, pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # CLI packages.
  environment.systemPackages = with pkgs; [
    (neovim.override ({
      withNodeJs = true;
      vimAlias = true;
      viAlias = true;
    }))
    ((emacsPackagesGen emacs).emacsWithPackages (epkgs: [
      epkgs.vterm
    ]))
    (mpv-with-scripts.override ({
      scripts = [ mpvScripts.mpris ];
    }))
    any-nix-shell
    aria2
    bc
    bind
    curl
    fd
    ffmpeg
    ffmpegthumbnailer
    file
    fzf
    gettext
    ghostscript
    gitFull
    glxinfo
    graphicsmagick-imagemagick-compat
    htop
    ispell
    jq
    linuxPackages.cpupower
    lm_sensors
    lshw
    lsof
    manpages
    mediainfo
    ncdu
    netcat-gnu
    nnn
    nox
    openssl
    p7zip
    pandoc
    pciutils
    playerctl
    powertop
    psmisc
    page
    pv
    python3Packages.youtube-dl
    ripgrep
    screenkey
    sloccount
    sshuttle
    stow
    sxiv
    tealdeer
    telnet
    tig
    universal-ctags
    unrar
    unzip
    usbutils
    wget
    wmctrl
    xclip
    zip
  ];

  # Fonts used in terminal.
  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      hack-font
      inconsolata
      iosevka
      powerline-fonts
      source-code-pro
      symbola
    ];
  };

  # Enable programs that need special configuration.
  programs = {
    iftop.enable = true;
    mtr.enable = true;
    tmux = {
      enable = true;
      terminal = "screen-256color";
      secureSocket = false;
    };
    zsh = {
      enable = true;
      promptInit = ''
        any-nix-shell zsh --info-right | source /dev/stdin
      '';
    };
  };

  # Enable services.
  services = {
    cron.enable = true;
  };
}
