{ config, lib, pkgs, ... }:

{
  # Emacs overlay
  nixpkgs.overlays = [
    (import (fetchGit {
      url = "https://github.com/nix-community/emacs-overlay";
      ref = "master";
      rev = "e3da699893c4be3b946d3586143b03450f9680ee";
    }))
  ];

  home.packages = with pkgs; [ emacs-all-the-icons-fonts ];

  programs.emacs = {
    enable = true;
    package = (pkgs.emacsPackagesGen pkgs.emacsPgtkGcc).emacsWithPackages
      (epkgs: [ epkgs.vterm ]);
  };

  # My Emacs configuration is very complex, using stow here
  home.activation.stowFiles = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    [ ! -d $HOME/.config/emacs ] && \
      $DRY_RUN_CMD ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs/ $HOME/.config/emacs
    $DRY_RUN_CMD ${pkgs.stow}/bin/stow -d $DOTFILES_PATH --ignore='.zsh' doom-emacs
  '';

  home.sessionVariables = { PATH = "$HOME/.config/emacs/bin:$PATH"; };

  programs.zsh = {
    shellAliases = {
      em = "run-bg emacs";
      et = "emacs -nw";
    };
    initExtra = ''
      emp() {
        local arg
        for arg in $@; do
          if [[ -d "$arg" ]]; then
            touch "$arg/.projectile"
          elif [[ -f "$arg" ]]; then
            local dirname=$(dirname )
            touch "$(dirname \"$arg\")/.projectile"
          fi
        done
        em $@
      }
    '';
  };
}
