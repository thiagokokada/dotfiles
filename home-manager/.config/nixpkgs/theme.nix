{ config, pkgs, ... }:

{
  my = {
    fonts = {
      gui = {
        package = pkgs.roboto;
        name = "Roboto";
      };
    };

    theme.colors = builtins.fromJSON (builtins.readFile ./theme.json);
  };

  # Enable fonts in home.packages to be available to applications
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; with config.my.fonts; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
    font-awesome_5
    gui.package
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];

  gtk = {
    enable = true;
    font = {
      package = pkgs.noto-fonts;
      name = "Noto Sans";
    };
    iconTheme = {
      package = pkgs.arc-icon-theme;
      name = "Arc";
    };
    theme = with config.my.theme.gtk; {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
  };
}
