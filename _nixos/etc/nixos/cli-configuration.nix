{ config, pkgs, ... }:

let
  unstable = import (builtins.fetchGit {
    name = "nixos-unstable-2020-04-08";
    url = https://github.com/nixos/nixpkgs/;
    rev = "f1090bdaf85581c4e9e1fecfcc30f30bbf7a04d6";
  }) {
    config = config.nixpkgs.config;
  };
in
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/thiagokokada/emacs-overlay/archive/harfbuzz.tar.gz;
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
    ((emacsPackagesNgGen emacsUnstable).emacsWithPackages (epkgs: [
      epkgs.emacs-libvterm
    ]))
    ag
    appimage-run
    aria2
    bc
    bind
    curl
    fd
    file
    fzf
    ghostscript
    gitFull
    glxinfo
    graphicsmagick-imagemagick-compat
    htop
    ispell
    jq
    libvterm-neovim
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
    tealdeer
    telnet
    tig
    unrar
    unstable.page
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
    zsh.enable = true;
  };

  # Enable services.
  services = {
    cron.enable = true;
  };
}
