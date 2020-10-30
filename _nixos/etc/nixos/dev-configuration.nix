{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    binutils
    cmake
    elixir
    erlang
    expect
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
    python-language-server
    python3Full
    python3Packages.black
    python3Packages.flake8
    rustup
    shellcheck
    xxd
  ];

  programs.adb.enable = true;

  virtualisation = {
    # Enable Docker.
    docker.enable = true;
  };

  # Added user to groups.
  users.users.thiagoko.extraGroups = [ "docker" ];
}
