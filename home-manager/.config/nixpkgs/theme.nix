{ config, pkgs, ... }:

{
  my.theme = {
    colors = {
      # https://github.com/chriskempson/base16-tomorrow-scheme/blob/master/tomorrow-night.yaml
      base00 = "#1D1F21";
      base01 = "#282A2E";
      base02 = "#373B41";
      base03 = "#969896";
      base04 = "#B4B7B4";
      base05 = "#C5C8C6";
      base06 = "#E0E0E0";
      base07 = "#FFFFFF";
      base08 = "#CC6666";
      base09 = "#DE935F";
      base0A = "#F0C674";
      base0B = "#B5BD68";
      base0C = "#8ABEB7";
      base0D = "#81A2BE";
      base0E = "#B294BB";
      base0F = "#A3685A";
    };
    icon = {
       name = "Arc";
       package = pkgs.arc-icon-theme;
    };
    gtk = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
  };

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
    pkgs.hack-font
  ];

  gtk = {
    enable = true;
    font = {
      package = pkgs.noto-fonts;
      name = "Noto Sans";
    };
    iconTheme = with config.my.theme.icon; {
      inherit name package;
    };
    theme = with config.my.theme.gtk; {
      inherit name package;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
  };
}
