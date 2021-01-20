{ config, lib, pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    package = with pkgs; rofi.override { plugins = [ rofi-calc rofi-emoji ]; };
    font = with config.my.fonts; "${gui.package} 14";
    theme = ../../../i3/.config/rofi/custom.rasi;
    extraConfig = ''
      rofi.show-icons: true
      rofi.modi: drun,emoji,ssh
      rofi.kb-row-up: Up,Control+k
      rofi.kb-row-down: Down,Control+j
      rofi.kb-accept-entry: Control+m,Return,KP_Enter
      rofi.kb-remove-to-eol: Control+Shift+e
      rofi.kb-mode-next: Shift+Right,Control+Tab,Control+l
      rofi.kb-mode-previous: Shift+Left,Control+Shift+Tab,Control+h
      rofi.kb-remove-char-back: BackSpace
    '';
  };
}
