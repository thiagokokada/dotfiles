{ config, lib, pkgs, ... }:
let
  # Aliases
  # Programs
  browser = "firefox";
  fileManager = "${terminal} ${pkgs.nnn}/bin/nnn";
  dex = "${pkgs.dex}/bin/dex";
  kbdd = "${pkgs.kbdd}/bin/kbdd";
  statusCommand = "${pkgs.i3pyblocks}/bin/i3pyblocks -c ${
      config.my.dotfiles-dir + "/i3/.config/i3pyblocks/config.py"
    }";
  # light needs to be installed in system, so not defining a path here
  light = "light";
  menu = "${config.programs.rofi.package}/bin/rofi";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  terminal = config.my.terminal;
  xset = "${pkgs.xorg.xset}/bin/xset";

  # TODO: Screenshots
  fullScreenShot = "";
  areaScreenShot = "";

  commonOptions = import ./i3-common.nix {
    inherit config lib terminal menu pactl light playerctl fullScreenShot
      areaScreenShot browser fileManager statusCommand;

    outputs = {
      HDMI-A-1 = { scale = 1.5; };
    };

    extraConfig = ''
      hide_edge_borders --i3 smart
    '';
  };
in {
  wayland.windowManager.sway = with commonOptions; {
    enable = true;

    inherit extraConfig;

    config = commonOptions.config // {
      startup = [{ command = "${dex} --autostart"; }];
    };

    extraSessionCommands = ''
      export GDK_BACKEND=wayland
      export SDL_VIDEODRIVER=wayland
      # needs qt5.qtwayland in systemPackages
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';

    wrapperFeatures = {
      base = true;
      gtk = true;
    };
  };
}
