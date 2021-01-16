{ ... }:

{
  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+shift+0" = "change_font_size_all 0";
    };
    settings = {
      font = "Hack";
      font_size = "12.0";
      # Colors
      foreground = "#C5C8C6";
      background = "#1D1F21";
      selection_background = "#373B41";
      selection_foreground = "#35C8C6";
      background_opacity = "0.9";
      # 16 color space
      color0  = "#1D1F21";
      color1  = "#CC6666";
      color2  = "#B5BD68";
      color3  = "#F0C674";
      color4  = "#81A2BE";
      color5  = "#B294BB";
      color6  = "#8ABEB7";
      color7  = "#C5C8C6";
      color8  = "#969896";
      color9  = "#CC6666";
      color10 = "#B5BD68";
      color11 = "#F0C674";
      color12 = "#81A2BE";
      color13 = "#B294BB";
      color14 = "#8ABEB7";
      color15 = "#FFFFFF";
      # 256 color space
      color16 = "#DE935F";
      color17 = "#A3685A";
      color18 = "#282A2E";
      color19 = "#373B41";
      color20 = "#B4B7B4";
      color21 = "#E0E0E0";
      # Scrollback
      scrollback_lines = 10000;
      scrollback_pager = "page -f";
      # Reduce lag
      sync_to_monitor = false;
      repaint_delay = 10;
      input_delay = 0;
      # Open URLs on click without modifier
      open_url_modifiers = "no_op";
      # Bell
      visual_bell_duration = "0.0";
      enable_audio_bell = false;
      window_alert_bell = true;
      bell_on_tab = true;
      editor = "nvim";
    };
  };

  programs.zsh.shellAliases = {
    icat = "kitty +kitten icat";
    ssh = "kitty +kitten ssh";
  };
}
