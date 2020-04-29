{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (yarn.override ({
      nodejs = nodejs-12_x;
    }))
    elixir
    erlang
    binutils
    cmake
    gcc
    gnumake
    libtool
    m4
    ncurses.dev
    nim
    nodejs-12_x
    pipenv
    python3Full
    python3Packages.black
    python3Packages.flake8
    rustup
    shellcheck
    xxd
  ];

  programs.adb.enable = true;
}
