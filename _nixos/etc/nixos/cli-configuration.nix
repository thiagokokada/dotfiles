{ pkgs, config, ... }:

let
  unstable = import (fetchGit {
    name = "nixos-unstable-2019-01-01";
    url = https://github.com/nixos/nixpkgs/;
    rev = "b58ccb43c991ccdb67ca7f11051a2ba1bc1d2ff2"; # neovim v0.3.2
  }) {
    config = config.nixpkgs.config;
  };
in
{
  # CLI packages.
  environment.systemPackages = with pkgs; [
    (python2Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (python3Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (unstable.neovim.override ({
      withNodeJs = true;
    }))
    aria2
    bc
    bind
    curl
    emacs
    fzf
    gcc
    gitFull
    gnumake
    htop
    jq
    lsof
    mediainfo
    pv
    python3Packages.youtube-dl
    ripgrep
    tmux
    universal-ctags
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
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      if [[ -n "''${commands[fzf-share]}" ]]; then
        source "$(fzf-share)/key-bindings.zsh"
      fi
    '';
  };
}
