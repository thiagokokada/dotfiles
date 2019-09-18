{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/a1ce0fbb298b93a119f2e883c1cdb05f40adc0c3.tar.gz;
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
