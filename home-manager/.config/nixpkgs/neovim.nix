{ config, lib, pkgs, ... }:

{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython = true;
    withPython3 = true;
    withRuby = true;
  };

  # Neovim module seems somewhat broken in 20.09, and can't backport
  # thanks to some breaking changes. Wait until 21.05.
  home.activation.stowNvim = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ${pkgs.stow}/bin/stow -d $DOTFILES_PATH --ignore='.zsh' nvim
  '';
}
