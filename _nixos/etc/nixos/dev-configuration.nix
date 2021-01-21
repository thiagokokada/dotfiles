{ pkgs, config, ... }:

let
  emacsCustom = (pkgs.emacsPackagesGen pkgs.emacsPgtkGcc).emacsWithPackages (epkgs: [
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
    (import (fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/e3da699893c4be3b946d3586143b03450f9680ee.tar.gz";
      sha256 = "1mld0agq52xhbhwfffjqrrpk0niyj0hkxjgy7ban0w0khla9ah4n";
    }))
  ];

  environment.systemPackages = with pkgs; [
    binutils
    cmake
    docker-compose
    elixir
    emacsCustom
    erlang
    expect
    gcc
    gitFull
    github-cli
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
    unstable.nix-update
    unstable.nixfmt
    unstable.nixpkgs-fmt
    unstable.nixpkgs-review
    unstable.rnix-lsp
    xxd
  ];

  # Enable adb
  programs.adb.enable = true;

  virtualisation = {
    # Enable Docker.
    docker.enable = true;
  };

  # Added user to groups.
  users.users.${config.my.username}.extraGroups = [ "docker" ];
}
