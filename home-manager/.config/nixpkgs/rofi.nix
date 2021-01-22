{ config, lib, pkgs, ... }:

let
  rofiRefactor = fetchGit {
    url = "https://github.com/thiagokokada/home-manager";
    ref = "migrate-rofi-to-rasi";
    rev = "75fab73330c4fc65c65c14e8e0ba86a4de4a2e2b";
  };
in {
  imports = [ "${rofiRefactor}/modules/programs/rofi.nix" ];

  disabledModules = [ <home-manager/modules/programs/rofi.nix> ];

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    package = with pkgs.unstable; rofi.override { plugins = [ rofi-calc rofi-emoji ]; };
    font = with config.my.fonts; "${gui.package} 14";
    theme = ../../../i3/.config/rofi/custom.rasi;
    extraConfig = {
      show-icons = true;
      modi = "drun,emoji,ssh";
      kb-row-up = "Up,Control+k";
      kb-row-down = "Down,Control+j";
      kb-accept-entry = "Control+m,Return,KP_Enter";
      kb-remove-to-eol = "Control+Shift+e";
      kb-mode-next = "Shift+Right,Control+Tab,Control+l";
      kb-mode-previous = "Shift+Left,Control+Shift+Tab,Control+h";
      kb-remove-char-back = "BackSpace";
    };
  };
}
