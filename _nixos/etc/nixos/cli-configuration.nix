{ pkgs, config, ... }:

let
  unstable = import (builtins.fetchGit {
    name = "nixos-unstable-2019-04-29";
    url = https://github.com/nixos/nixpkgs/;
    rev = "496f9309480b22173e25885bc7c128c30fbd4da3"; # emacs 26.2
  }) {
    config = config.nixpkgs.config;
  };
in

{
  nixpkgs.config.allowUnfree = true;

  # CLI packages.
  environment.systemPackages = with pkgs; [
    (mpv-with-scripts.override ({
      scripts = [ mpvScripts.mpris ];
    }))
    (python2Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (python3Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (neovim.override ({
      withNodeJs = true;
      vimAlias = true;
      viAlias = true;
    }))
    ag
    aria2
    bc
    bind
    cmus
    curl
    daemonize
    fd
    file
    fzf
    gcc
    gitFull
    glxinfo
    gnumake
    htop
    ispell
    jq
    linuxPackages.cpupower
    lshw
    lsof
    manpages
    mediainfo
    mosh
    mtr
    ncdu
    ncurses.dev
    netcat-gnu
    openssl
    p7zip
    pandoc
    parted
    pciutils
    powertop
    psmisc
    pv
    python3Packages.youtube-dl
    ripgrep
    shellcheck
    sshuttle
    stow
    telnet
    tig
    universal-ctags
    unrar
    unstable.emacs
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
