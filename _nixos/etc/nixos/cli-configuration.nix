{ pkgs, ... }:

{
  # CLI packages.
  environment.systemPackages = with pkgs; [
    (python2Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (python3Full.withPackages(ps: with ps; [ pip tkinter virtualenv ]))
    (neovim.override ({
      vimAlias = true;
      viAlias = true;
    }))
    aria2
    bc
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
