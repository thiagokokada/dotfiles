{ config, lib, pkgs, ... }:
let
  # Aliases
  alt = "Mod1";
  modifier = "Mod4";

  commonOptions = let
    makoctl = "${pkgs.mako}/bin/makoctl";

    menuScript = with config.my.fonts;
      with config.my.theme.colors;
      pkgs.writeShellScriptBin "menu" ''
        opts=(
          '--list' '15'
          '--ignorecase'
          '--wrap'
          '--prompt' 'run'
          '--fn' '"${gui.name} 12"'
          '--tb' '${base00}'
          '--tf' '${base0D}'
          '--fb' '${base00}'
          '--ff' '${base03}'
          '--nb' '${base00}'
          '--nf' '${base05}'
          '--hf' '${base00}'
          '--hb' '${base0D}'
        )
        export BEMENU_OPTS="''${opts[@]}"
        ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop --dmenu='${pkgs.bemenu}/bin/bemenu'
      '';
    screenShotName = with config.xdg.userDirs;
      "${pictures}/$(${pkgs.coreutils}/bin/date +%Y-%m-%d_%H-%M-%S)-screenshot.png";
  in import ./i3-common.nix rec {
    inherit config lib modifier alt;
    browser = "firefox";
    fileManager = "${terminal} ${pkgs.nnn}/bin/nnn";
    statusCommand = with config;
      "${programs.i3status-rust.package}/bin/i3status-rs ${xdg.configHome}/i3status-rust/config-sway.toml";
    menu = "${menuScript}/bin/menu";
    # light needs to be installed in system, so not defining a path here
    light = "light";
    pactl = "${pkgs.pulseaudio}/bin/pactl";
    playerctl = "${pkgs.playerctl}/bin/playerctl";
    terminal = config.my.terminal;

    fullScreenShot = ''
      ${pkgs.grim}/bin/grim "${screenShotName}" && \
      ${pkgs.libnotify}/bin/notify-send -u normal -t 5000 'Full screenshot taken'
    '';
    areaScreenShot = ''
      ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" "${screenShotName}" && \
      ${pkgs.libnotify}/bin/notify-send -u normal -t 5000 'Area screenshot taken'
    '';

    # TODO: Not working it seems?
    extraBindings = {
      "Ctrl+space" = "${makoctl} dismiss";
      "Ctrl+Shift+space" = "${makoctl} dismiss -a";
    };

    extraConfig = ''
      hide_edge_borders --i3 smart
    '';
  };
in {
  imports = [ ./i3status-rust.nix ./mako.nix ];

  wayland.windowManager.sway = with commonOptions; {
    enable = true;

    inherit extraConfig;

    config = commonOptions.config // {
      startup = [
        { command = "${pkgs.dex}/bin/dex --autostart"; }
        {
          command = let
            swayidle = "${pkgs.swayidle}/bin/swayidle";
            swaylock = "${pkgs.swaylock}/bin/swaylock";
            swaymsg = "${pkgs.sway}/bin/swaymsg";
          in ''
            ${swayidle} -w \
            timeout 600 '${swaylock} -f -c 000000' \
            timeout 605 '${swaymsg} "output * dpms off"' \
            resume '${swaymsg} "output * dpms on"' \
            before-sleep '${swaylock} -f -c 000000' \
            lock '${swaylock} -f -c 000000'
          '';
        }
      ];

      input = {
        "type:keyboard" = {
          xkb_layout = "us(intl),br";
          xkb_options = "caps:escape,grp:win_space_toggle";
        };
        "type:pointer" = { accel_profile = "flat"; };
        "type:touchpad" = {
          drag = "enabled";
          drag_lock = "enabled";
          middle_emulation = "enabled";
          natural_scroll = "enabled";
          scroll_method = "two_finger";
          tap = "enabled";
          tap_button_map = "lmr";
        };
      };

      output = { "*" = { scale = "1.35"; }; };
    };

    extraSessionCommands = ''
      # Breaks Chromium/Electron
      # export GDK_BACKEND=wayland
      # Firefox
      export MOZ_ENABLE_WAYLAND=1
      # Qt
      export XDG_SESSION_TYPE=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      # SDL
      export SDL_VIDEODRIVER=wayland
      # Elementary/EFL
      export ECORE_EVAS_ENGINE=wayland_egl
      export ELM_ENGINE=wayland_egl
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';

    systemdIntegration = true;

    wrapperFeatures = {
      base = true;
      gtk = true;
    };
  };

  services = { udiskie.enable = true; };

  home.packages = with pkgs; [ bemenu dex mako swayidle swaylock wl-clipboard ];
}
