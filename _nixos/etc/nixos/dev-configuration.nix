{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    binutils
    cmake
    elixir
    erlang
    gcc
    gnumake
    go
    joker
    libtool
    m4
    ncurses.dev
    nim
    nodejs-10_x
    pipenv
    python3Full
    python3Packages.black
    python3Packages.flake8
    rustup
    shellcheck
    unstable.python-language-server
    xxd
  ];

  programs.adb.enable = true;
}
