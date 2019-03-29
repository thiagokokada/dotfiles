{ pkgs, config, ... }:

let
  unstable = import (fetchGit {
    name = "nixos-unstable-2019-01-06";
    url = https://github.com/nixos/nixpkgs/;
    rev = "b4ed953bb2f7486173ebc062296a1c2a04933b34"; # neovim v0.3.3
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
    (unstable.neovim.override ({
      withNodeJs = true;
    }))
    ag
    aria2
    bc
    bind
    cmus
    curl
    daemonize
    emacs
    fd
    file
    fzf
    gcc
    gitFull
    gnumake
    htop
    ispell
    jq
    lsof
    manpages
    mediainfo
    mosh
    mtr
    ncdu
    netcat-gnu
    p7zip
    pandoc
    parted
    pv
    python3Packages.youtube-dl
    ripgrep
    shellcheck
    sshuttle
    stow
    telnet
    tig
    tmux
    universal-ctags
    unrar
    unzip
    usbutils
    vim
    wget
    xclip
  ];

  # Fonts used in terminal.
  fonts = {
    fonts = with pkgs; [
      hack-font
      inconsolata
      powerline-fonts
      source-code-pro
    ];
  };

  # Enable ZSH.
  programs = {
    iftop.enable = true;
    zsh = {
      enable = true;
      interactiveShellInit = ''
        if [[ -n "''${commands[fzf-share]}" ]]; then
          source "$(fzf-share)/key-bindings.zsh"
        fi
      '';
    };
  };

  services = {
    cron.enable = true;
  };
}
