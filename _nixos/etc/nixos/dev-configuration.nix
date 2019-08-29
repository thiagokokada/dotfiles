{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (yarn.override ({
      nodejs = nodejs-10_x;
    }))
    binutils
    cmake
    gcc
    gnumake
    m4
    ncurses.dev
    nodejs-10_x
    pipenv
    python2Full
    python3Full
    python3Packages.poetry
    shellcheck
    universal-ctags
    xxd
  ];
}
