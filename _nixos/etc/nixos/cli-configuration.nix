{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/4703059bca8b225e3f7d3e0c7dc845d53831b0e1.tar.gz;
    }))
  ];

  # CLI packages.
  environment.systemPackages = with pkgs; [
    (mpv-with-scripts.override ({
      scripts = [ mpvScripts.mpris ];
    }))
    (neovim.override ({
      withNodeJs = true;
      vimAlias = true;
      viAlias = true;
    }))
    ag
    appimage-run
    aria2
    bind
    bc
    curl
    emacsGit
    fd
    file
    fzf
    gitFull
    glxinfo
    graphicsmagick-imagemagick-compat
    htop
    ispell
    jq
    linuxPackages.cpupower
    lshw
    lsof
    manpages
    mediainfo
    ncdu
    netcat-gnu
    nox
    openssl
    p7zip
    pandoc
    pciutils
    powertop
    psmisc
    pv
    python3Packages.youtube-dl
    ripgrep
    sshuttle
    stow
    telnet
    tig
    unrar
    unzip
    usbutils
    wget
    xclip
  ];

  # Fonts used in terminal.
  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      hack-font
      inconsolata
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
    zsh.enable = true;
  };

  # Enable services.
  services = {
    cron.enable = true;
  };
}
