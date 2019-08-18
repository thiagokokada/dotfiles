{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  # CLI packages.
  environment.systemPackages = with pkgs; [
    (mpv-with-scripts.override ({
      scripts = [ mpvScripts.mpris ];
    }))
    (python2Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (python3Full.withPackages(ps: with ps; [ pip tkinter ]))
    (neovim.override ({
      withNodeJs = true;
      vimAlias = true;
      viAlias = true;
    }))
    (yarn.override ({
      nodejs = nodejs-10_x;
    }))
    ag
    aria2
    bc
    bind
    binutils
    cmus
    curl
    daemonize
    emacs
    fd
    file
    fzf
    gcc
    gitFull
    glxinfo
    gnumake
    htop
    imagemagick7Big
    ispell
    jq
    linuxPackages.cpupower
    lshw
    lsof
    m4
    manpages
    mediainfo
    mosh
    mtr
    ncdu
    ncurses.dev
    netcat-gnu
    nodejs-10_x
    openssl
    p7zip
    pandoc
    parted
    pciutils
    pipenv
    powertop
    psmisc
    pv
    python3Packages.poetry
    python3Packages.youtube-dl
    ripgrep
    shellcheck
    sshuttle
    stow
    telnet
    tig
    universal-ctags
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
