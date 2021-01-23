{ config, lib, pkgs, ... }:
let
  # Aliases
  alt = "Mod1";
  modifier = "Mod4";

  # Modes
  displayLayoutMode =
    " : [h]  , [j]  , [k]  , [l]  , [d]uplicate, [m]irror, [s]econd-only, [o]ff";

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
  rofi = "${config.programs.rofi.package}/bin/rofi";
  menu = "${rofi} -show drun";
  mons = "${pkgs.mons}/bin/mons";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  terminal = config.my.terminal;
  xset = "${pkgs.xorg.xset}/bin/xset";

  # Screenshots
  screenShotName =
    "$(${pkgs.coreutils}/bin/date +%Y-%m-%d_%H-%M-%S)-screenshot.png";
  fullScreenShot = with config.xdg.userDirs; ''
    ${pkgs.maim}/bin/maim "${pictures}/${screenShotName}" && \
    ${pkgs.libnotify}/bin/notify-send -u normal -t 5000 'Full screenshot taken'
  '';
  areaScreenShot = with config.xdg.userDirs; ''
    ${pkgs.maim}/bin/maim -s "${pictures}/${screenShotName}" && \
    ${pkgs.libnotify}/bin/notify-send -u normal -t 5000 'Area screenshot taken'
  '';

  commonOptions = import ./i3-common.nix {
    inherit config lib terminal menu pactl modifier alt light playerctl
      fullScreenShot areaScreenShot browser fileManager statusCommand;

    extraBindings = {
      "${modifier}+p" = ''mode "${displayLayoutMode}"'';
      "${modifier}+c" =
        "exec ${rofi} -show calc -modi calc -no-show-match -no-sort";
      "${modifier}+Tab" = "exec ${rofi} -show window -modi window";
    };

    extraModes = with commonOptions.helpers; {
      ${displayLayoutMode} = (mapDirection {
        leftCmd = "mode default, exec ${mons} -e left";
        downCmd = "mode default, exec ${mons} -e bottom";
        upCmd = "mode default, exec ${mons} -e top";
        rightCmd = "mode default, exec ${mons} -e right";
      }) // {
        d = "mode default, exec ${mons} -d";
        m = "mode default, exec ${mons} -m";
        s = "mode default, exec ${mons} -s";
        o = "mode default, exec ${mons} -o";
        "Escape" = "mode default";
        "Return" = "mode default";
      };
    };
  };
in {
  imports = [ ./dunst.nix ./rofi.nix ];

  nixpkgs.overlays = [
    (import (fetchGit {
      url = "https://github.com/thiagokokada/i3pyblocks";
      ref = "nix-overlay";
    }))
  ];

  xsession.enable = true;
  xsession.windowManager.i3 = with commonOptions; {
    enable = true;

    inherit extraConfig;

    config = commonOptions.config // {
      startup = [
        {
          command = "${xset} s 600";
          notification = false;
        }
        {
          command = "${dex} --autostart";
          notification = false;
        }
      ];

    };
  };

  xdg.userDirs.enable = true;

  services = { udiskie.enable = true; };

  systemd.user.services = {
    kbdd = {
      Unit = {
        Description = "kbdd daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.kbdd}/bin/kbdd -n";
        Type = "dbus";
        BusName = "ru.gentoo.KbddService";
        Restart = "on-failure";
      };
    };

    xss-lock = {
      Unit = {
        Description = "Use external locker as X screen saver";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart =
          "${pkgs.xss-lock}/bin/xss-lock -s $XDG_SESSION_ID -l -- ${pkgs.lockscreen}/bin/lock-screen";
        Restart = "on-failure";
      };
    };
  };
}
