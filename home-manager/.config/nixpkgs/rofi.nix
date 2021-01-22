{ config, lib, pkgs, ... }:

let
  rofiRefactor = fetchGit {
    url = "https://github.com/thiagokokada/home-manager";
    ref = "migrate-rofi-to-rasi";
    rev = "75fab73330c4fc65c65c14e8e0ba86a4de4a2e2b";
  };

  theme = with config.my.theme.colors; ''
    * {
      background-color: ${base00};
      border-color: ${base01};
      text-color: ${base05};
      spacing: 0;
      width: 512px;
    }

    inputbar {
      border: 0 0 1px 0;
      children: [prompt,entry];
    }

    prompt {
      padding: 16px;
      border: 0 1px 0 0;
    }

    textbox {
      background-color: ${base01};
      border: 0 0 1px 0;
      border-color: ${base00};
      padding: 8px 16px;
    }

    entry {
      padding: 16px;
    }

    listview {
      cycle: true;
      margin: 0 0 -1px 0;
      scrollbar: false;
    }

    element {
      border: 0 0 1px 0;
      padding: 8px;
    }

    element selected {
      background-color: ${base0D};
      color: ${base00};
    }
  '';
in {
  imports = [ "${rofiRefactor}/modules/programs/rofi.nix" ];

  disabledModules = [ <home-manager/modules/programs/rofi.nix> ];

  xdg.dataFile = { "rofi/themes/custom.rasi".text = theme; };

  programs.rofi = {
    enable = true;
    # theme = ../../../i3/.config/rofi/custom.rasi;
    theme = "custom";
    terminal = "${pkgs.kitty}/bin/kitty";
    package = with pkgs.unstable;
      rofi.override { plugins = [ rofi-calc rofi-emoji ]; };
    font = with config.my.fonts; "${gui.package} 14";
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
