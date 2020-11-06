{ pkgs, ... }:

let
  emacsCustom = (pkgs.emacsPackagesGen pkgs.emacsGcc).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]);
  neovimCustom = pkgs.neovim.override ({
    withNodeJs = true;
    vimAlias = true;
    viAlias = true;
  });
in {
  # Emacs overlay
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/b3f0332e988f29934623910c502e711832e9c4e4.tar.gz;
    }))
  ];

  environment.systemPackages = with pkgs; [
    binutils
    cmake
    elixir
    emacsCustom
    erlang
    expect
    gcc
    gnumake
    go
    joker
    libtool
    m4
    ncurses.dev
    neovimCustom
    nim
    nodejs-10_x
    pipenv
    python-language-server
    python3Full
    python3Packages.black
    python3Packages.flake8
    rustup
    shellcheck
    xxd
  ];

  # Enable adb
  programs.adb.enable = true;

  virtualisation = {
    # Enable Docker.
    docker.enable = true;
  };

  # Added user to groups.
  users.users.thiagoko.extraGroups = [ "docker" ];
}
