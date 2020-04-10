{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
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
    python3Full
    shellcheck
    xxd
  ];

  programs.adb.enable = true;
}
