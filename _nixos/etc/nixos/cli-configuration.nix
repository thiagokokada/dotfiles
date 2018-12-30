{ pkgs, ... }:

{
  # CLI packages.
  environment.systemPackages = with pkgs; [
    (python2Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (python3Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    curl
    fzf
    gcc
    gitFull
    gnumake
    htop
    jq
    mediainfo
    neovim
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
  programs.zsh.enable = true;
}
