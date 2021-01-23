{ config, lib, pkgs, ... }:

let
  rofiModule = fetchGit {
    url = "https://github.com/thiagokokada/home-manager";
    ref = "rofi-add-support-to-custom-themes";
    rev = "c654c5c6b7943079a8f87d00256665921be5fecb";
  };
in
{
  imports = [ "${rofiModule}/modules/programs/rofi.nix" ];

  disabledModules = [ <home-manager/modules/programs/rofi.nix> ];

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    package = with pkgs.unstable;
      rofi.override { plugins = [ rofi-calc rofi-emoji ]; };
    font = with config.my.fonts; "${gui.package} 14";
    theme = with config.my.theme.colors; {
      "*" = {
        background-color = base00;
        border-color = base01;
        text-color = base05;
        spacing = 0;
        width = "512px";
      };

      inputbar = {
        border = "0 0 1px 0";
        children = "[prompt,entry]";
      };

      prompt = {
        padding = "16px";
        border = "0 1px 0 0";
      };

      textbox = {
        background-color = base01;
        border = "0 0 1px 0";
        border-color = base00;
        padding = "8px 16px";
      };

      entry = {
        padding = "16px";
      };

      listview = {
        cycle = "true";
        margin = "0 0 -1px 0";
        scrollbar = "false";
      };

      element = {
        border = "0 0 1px 0";
        padding = "8px";
      };

      "element selected" = {
        background-color = base0D;
        color = base00;
      };
    };
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
