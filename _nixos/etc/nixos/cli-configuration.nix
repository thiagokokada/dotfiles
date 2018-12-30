{ pkgs, ... }:

{
  # CLI packages.
  environment.systemPackages = with pkgs; [
    (python27Full.withPackages(ps: with ps; [ pip requests tkinter virtualenv ]))
    (python36Full.withPackages(ps: with ps; [ pip requests tkinter virtualenv ]))
    curl
    fzf
    gcc
    gitFull
    gnumake
    htop
    mpv
    neovim
    vim
    wget
  ];

  # Enable ZSH.
  programs.zsh.enable = true;
}
