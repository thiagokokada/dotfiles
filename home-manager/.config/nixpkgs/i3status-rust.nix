{ config, lib, pkgs, ... }:

let
  i3statusRsModule = fetchGit {
    url = "https://github.com/nix-community/home-manager";
    ref = "master";
    rev = "9e9d8ffc7ca70840858153a179cb0dd4d869ea0a";
  };
in {
  imports = [ "${i3statusRsModule}/modules/programs/i3status-rust.nix" ];

  programs.i3status-rust = {
    enable = true;
    package = pkgs.unstable.i3status-rust;
    bars = let
      isNotebook = config.my.deviceType == "notebook";

      settings = {
        theme = {
          name = "plain";
          overrides = with config.my.theme.colors; {
            idle_bg = base00;
            idle_fg = base05;
            info_bg = base0D;
            info_fg = base00;
            good_bg = base00;
            good_fg = base05;
            warning_bg = base0A;
            warning_fg = base00;
            critical_bg = base08;
            critical_fg = base00;
            separator_bg = base00;
            separator = " ";
          };
        };
        icons = {
          name = "awesome5";
          overrides = {
            eco_on = "  ";
            eco_off = "  ";
          };
        };
      };

      windowBlock = {
        block = "focused_window";
        max_width = 50;
        show_marks = "visible";
      };
      netBlock = {
        block = "net";
        hide_missing = true;
        hide_inactive = true;
      };

      diskBlock = {
        block = "disk_space";
        path = "/";
        alias = "/";
        info_type = "available";
        unit = "GiB";
        format = "{icon}{available}{unit}";
      };

      memoryBlock = {
        block = "memory";
        format_mem = "{MFg}G";
        format_swap = "{SFg}G";
      };

      loadBlock = { block = "load"; };

      temperatureBlock = {
        block = "temperature";
        format = "{average}°C";
        collapsed = false;
        chip = "coretemp-*";
        good = 20;
        idle = 55;
        info = 70;
        warning = 80;
      };

      backlightBlock = if isNotebook then { block = "backlight"; } else { };

      batteryBlock = if isNotebook then {
        block = "battery";
        device = "DisplayDevice";
        driver = "upower";
      } else
        { };

      soundBlock = {
        block = "sound";
        on_click = "pavucontrol";
      };

      keyboardBlock = {
        block = "keyboard_layout";
        format = " {layout}";
        driver = "kbddbus";
      };

      dpmsBlock = let xset = "${pkgs.xorg.xset}/bin/xset";
      in {
        block = "toggle";
        command_state = "${xset} q | grep -Fo 'DPMS is Enabled'";
        command_on = "${xset} s on +dpms";
        command_off = "${xset} s off -dpms";
        icon_on = "eco_on";
        icon_off = "eco_off";
        interval = 5;
      };

      timeBlock = {
        block = "time";
        interval = 1;
        format = "%a %T";
      };

    in {
      i3 = {
        inherit settings;
        blocks = [
          windowBlock
          netBlock
          diskBlock
          memoryBlock
          loadBlock
          temperatureBlock
          dpmsBlock
          backlightBlock
          batteryBlock
          soundBlock
          keyboardBlock
          timeBlock
        ];
      };

      sway = {
        inherit settings;
        blocks = [
          windowBlock
          netBlock
          diskBlock
          memoryBlock
          loadBlock
          temperatureBlock
          backlightBlock
          batteryBlock
          soundBlock
          (keyboardBlock // { driver = "sway"; })
          timeBlock
        ];
      };
    };
  };
}
