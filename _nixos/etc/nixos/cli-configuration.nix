{ config, pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # CLI packages.
  environment.systemPackages = with pkgs; [
    (neovim.override ({
      withNodeJs = true;
      vimAlias = true;
      viAlias = true;
    }))
    ((emacsPackagesGen emacsUnstable).emacsWithPackages (epkgs: [
      epkgs.emacs-libvterm
    ]))
    (mpv-with-scripts.override ({
      scripts = [ mpvScripts.mpris ];
    }))
    (unstable.nnn.overrideAttrs (oldAttrs: {
      src = fetchFromGitHub {
        owner = "jarun";
        repo = "nnn";
        rev = "627c5cfc36461dc20f1e8c903148324fa15b063b";
        sha256 = "1kil2kx3wqbdzdshhg8js903528gl7zpa1i72nl7k9ypr1qkza4m";
      };
      buildFlags = [ "O_NERD=1" ];
    }))
    any-nix-shell
    aria2
    bc
    bind
    curl
    fd
    ffmpeg
    ffmpegthumbnailer
    file
    fzf
    gettext
    ghostscript
    gitFull
    glxinfo
    graphicsmagick-imagemagick-compat
    htop
    ispell
    jq
    linuxPackages.cpupower
    lm_sensors
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
    playerctl
    powertop
    psmisc
    pv
    python3Packages.youtube-dl
    ripgrep
    screenkey
    sloccount
    sshuttle
    stow
    sxiv
    tealdeer
    telnet
    tig
    universal-ctags
    unrar
    unstable.page
    unzip
    usbutils
    wget
    wmctrl
    xclip
    zip
  ];

  # Fonts used in terminal.
  fonts = {
    fonts = with pkgs; [
      (unstable.nerdfonts.override {
        fonts = [ "Hack" ];
      })
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
    zsh = {
      enable = true;
      promptInit = ''
        any-nix-shell zsh --info-right | source /dev/stdin
      '';
    };
  };

  # Enable services.
  services = {
    cron.enable = true;
  };
}
