{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;

  let
    custom-python-packages = python-packages: with python-packages; [
      black
      flake8
    ];

    python3Custom = python3Full.withPackages custom-python-packages;
  in
  [
    (yarn.override ({
      nodejs = nodejs-10_x;
    }))
    binutils
    cmake
    elixir
    gcc
    gnumake
    libtool
    m4
    ncurses.dev
    nodejs-10_x
    pipenv
    python2Full
    python3Custom
    rustup
    shellcheck
    xxd
  ];

  programs.adb.enable = true;
}
