{ config, pkgs, ... }:

{
  imports = [
    ./emacs.nix
    ./git.nix
    ./i3.nix
    ./kitty.nix
    ./misc.nix
    ./mpv.nix
    ./my.nix
    ./neovim.nix
    ./nixos.nix
    ./nnn.nix
    ./ssh.nix
    ./theme.nix
    ./tmux.nix
    ./zsh.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "thiagoko";
  home.homeDirectory = "/home/thiagoko";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
